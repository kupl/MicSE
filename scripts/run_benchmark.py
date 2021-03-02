import os
import sys
import time
import argparse
import tempfile
import subprocess
import multiprocessing
from datetime import datetime
from dotenv import load_dotenv
from slack_sdk import WebClient

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.join (BASE_DIR, "..")
BENCHMARK_DIR = os.path.join (PROJECT_DIR, "benchmarks")
OUTPUT_DIR = os.path.join (BASE_DIR, "output")
DOTENV_PATH = os.path.join (BASE_DIR, ".env")

def main ():
  try:
    sys.stderr.write ("> Start\n")
    env = read_env ()
    multi_run (env)
    sys.stderr.write ("> Done\n")
  except Exception as ex:
    excType, excValue, excTrackback = sys.exc_info ()
    sys.stderr.write ("> Error: {}\n\n".format (ex))
    traceback.print_tb (excTrackback, file=sys.stderr)
  return None


def read_env ():
  env = dict()
  # Parse arguments
  parser = argparse.ArgumentParser ()
  parser.add_argument ("-b", "--benchmark", type=str, default="toy", help="benchmark directory which to examinate (default: toy)")
  parser.add_argument ("-o", "--output", type=str, default="", help="output file name (default: same with benchmark option)")
  parser.add_argument ("-z", "--z3-time-budget", type=int, default=30, help="time budget for Z3 solver in second (default: 30s)")
  parser.add_argument ("-p", "--prover-time-budget", type=int, default=180, help="time budget for whole prover process in second (default: 180s)")
  parser.add_argument ("-r", "--refuter-time-budget", type=int, default=180, help="time budget for each loop of refuter process in second (default: 180s)")
  parser.add_argument ("-l", "--unroll-loop", type=int, default=1, help="number of loop unrolling (default: 1)")
  parser.add_argument ("-t", "--trx-len", type=int, default=1, help="maximum length of transaction scenario (default: 1)")
  parser.add_argument ("-m", "--multi-process", type=int, default=max(multiprocessing.cpu_count() - 1, 1), help="number of process which are running parallely (default: number of core - 1)")
  parser.add_argument ("--no-post-result", dest="post", action="store_false", help="do not post result to slack channel when script is ended")
  parser.add_argument ("--initial-storage", dest="init_strg", action="store_true", help="set the initial storage on (file)_storage.tz")
  parser.set_defaults (post=True)
  parser.set_defaults (init_strg=False)
  args = parser.parse_args ()
  # Initialize env
  env = dict()
  ## Add env for benchmark directory path and candidates
  env['id'] = args.benchmark
  env['time'] = time.time ()
  env['benchmark'] = os.path.join (BENCHMARK_DIR, args.benchmark)
  env['init_strg'] = args.init_strg
  if not (os.path.isdir (env['benchmark'])):
    raise Exception ("Wrong benchmark directory")
  else:
    listdir = os.listdir(env['benchmark'])
    listtz = list(filter(lambda x: x.endswith ('.tz'), listdir))
    candidates = list(filter(lambda x: not x.endswith ('_storage.tz'), listdir))
    env['candidates'] = sorted(candidates)
  ## Add env for output file path
  if not (os.path.isdir (OUTPUT_DIR)):
    os.mkdir(OUTPUT_DIR)
  if args.output == "":
    env['output'] = os.path.join (OUTPUT_DIR, args.benchmark)
  else:
    env['output'] = os.path.join (OUTPUT_DIR, args.output)
  env['output'] += "_{}.out".format (time.strftime ("%Y%m%d%H%M%S", time.gmtime(env['time'])))
  ## Add env for time budget
  env['z3_time'] = args.z3_time_budget
  env['prover_time'] = args.prover_time_budget
  env['refuter_time'] = args.refuter_time_budget
  env['unroll_loop'] = args.unroll_loop
  env['trx_len'] = args.trx_len
  ## Add env for multiprocess
  env['multi_process'] = max (1, min (args.multi_process, multiprocessing.cpu_count() - 1))
  ## Add env for slack posting
  env['post'] = args.post
  if env['post']:
    load_dotenv (dotenv_path=DOTENV_PATH)
    env['token'] = os.getenv ("SLACK_BOT_TOKEN")
    env['channel'] = os.getenv ("SLACK_CHANNEL")
    env['client'] = WebClient (token=env['token'])
    env['body'] = post_start (env)
  return env


def multi_run (env):
  jobLen = len(env['candidates'])
  # Check make failure
  proc = subprocess.run ("make", stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True, cwd=PROJECT_DIR)
  if proc.returncode != 0:
    print (proc.stderr)
    return None
  # Synchronized Value Manager for multiprocessing Pool
  manager = multiprocessing.Manager()
  runrst = manager.list([]) # container to save run-results
  syncdata = manager.list([0,jobLen]) # (0 : terminatedJobCount, 1 : jobLen)
  # Make a multiprocessing Pool and run all
  idx_candidate_env_runrst_syncdata_list = [(idx, candidate, env, runrst, syncdata) for idx, candidate in enumerate(env['candidates'])]
  pool = multiprocessing.Pool(processes=env['multi_process'])
  pool.starmap(run_f, idx_candidate_env_runrst_syncdata_list)
  # I don't know why, but close() and join() recommended at "https://stackoverflow.com/a/47683305"
  pool.close()
  pool.join()
  # Write Outputs
  finish_time = time.time ()
  runrst = sorted (runrst, key=lambda job: job['idx'])
  with open (env['output'], 'w') as fp:
    for job in runrst:
      fp.write(job['resultLog'])
  post_update (env, finish_time)
  return None


def run_f (idx, file, env, runrst, syncdata):
  # Set command line
  #cmd = ["dune", "exec", "--", "micse"]
  file = os.path.join (env['benchmark'], file)
  strg_file = os.path.join (env['benchmark'], file).replace (".tz", "_storage.tz")
  cmd = ["./_build/install/default/bin/micse"]
  cmd += ["--input", file]
  cmd += ["--inst-count"]
  cmd += ["--z3-timeout", str (env['z3_time'])]
  cmd += ["--prover-timeout", str (env['prover_time'])]
  cmd += ["--refuter-timeout-s", str (env['refuter_time'])]
  cmd += ["--unroll-l", str (env["unroll_loop"])]
  cmd += ["--unroll-t", str (env["trx_len"])]
  if env['init_strg'] and os.path.isfile (strg_file):
    cmd += ["--initial-storage", strg_file]
  # Job Started
  job = dict()
  job['file'] = file
  job['idx'] = idx
  sys.stderr.write ("  > Job #{} is started\n".format (job['idx']))
  tempFP = tempfile.TemporaryFile ()
  job['cmd'] = cmd
  job['startTime'] = time.time ()
  proc = subprocess.Popen (cmd, stdout=tempFP, stderr=subprocess.STDOUT, text=True, cwd=PROJECT_DIR)
  # Waits until the process finished
  job['process_end_status'] = proc.wait()
  # Job Finished
  job['endTime'] = time.time ()
  job['runTime'] = round ((job['endTime'] - job['startTime']), 3)
  job['terminateCheck'] = True
  syncdata[0] += 1
  # Print Log and append results
  tempFP.seek(0)
  job['resultLog'] = ( \
    '% ' + ' '.join (job['cmd']) + '\n' \
    + tempFP.read().decode() \
    + "...[{}s - {}/{}]\n\n".format (job['runTime'], job['idx'] + 1, syncdata[1])
  )
  tempFP.close()
  sys.stderr.write ("  > Job #{} is done in {}s [{}/{}]\n".format (job['idx'], job['runTime'], syncdata[0], syncdata[1]))
  runrst.append(job)
  post_result (job, env)
  return None

def post_start (env):
  if not env['post']:
    return None
  body = env['client'].chat_postMessage (
    channel=env['channel'],
    text="> Jobs for the benchmark *`{}`* is started at _{}_."
      .format (
        env['id'],
        time.strftime ("%Y-%m-%d %H:%M:%S", time.gmtime(env['time']))))
  return body

def post_result (job, env):
  if not env['post']:
    return None
  env['client'].files_upload(
    channels=env['channel'],
    thread_ts=env['body']['ts'],
    initial_comment="Job #{} for `{}`".format (job['idx'], job['file']),
    filename="{}_{}_{}.out".format (env['id'], job['idx'], time.strftime ("%Y%m%d%H%M%S", time.gmtime(env['time']))),
    filetype="text",
    title="Result Log for '{}'".format (job['file']),
    content=job['resultLog'])
  return None

def post_update (env, ftime):
  if not env['post']:
    return None
  env['client'].chat_update (
    channel=env['channel'], 
    ts=env['body']['ts'],
    text="> Jobs for the benchmark *`{}`* is done at _{}_.\n```\nStart Time: {}\nElapsed Time: {}sec\nOptions:\n  Z3 Time Budget: {}sec\n  Prover Time Budget: {}sec\n  Refuter Time Budget: {}sec\n  Max Unrolled Loop: {}\n  Max Transaction Length: {}\n  Multi Process: {}\n```"
      .format (
        env['id'], 
        time.strftime ("%Y-%m-%d %H:%M:%S", time.gmtime(ftime)),
        time.strftime ("%Y-%m-%d %H:%M:%S", time.gmtime(env['time'])),
        ftime - env['time'],
        env['z3_time'],
        env['prover_time'],
        env['refuter_time'],
        env['unroll_loop'],
        env['trx_len'],
        env['multi_process']))
  return None

if __name__ == "__main__":
  main ()