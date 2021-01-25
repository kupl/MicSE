import os
import sys
import time
import argparse
import tempfile
import subprocess
import multiprocessing
from datetime import datetime

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.join (BASE_DIR, "..")
BENCHMARK_DIR = os.path.join (PROJECT_DIR, "benchmarks")
OUTPUT_DIR = os.path.join (BASE_DIR, "output")


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
  parser.add_argument ("-r", "--refuter-time-budget", type=int, default=180, help="time budget for whole refuter process in second (default: 180s)")
  parser.add_argument ("-l", "--unroll-loop", type=int, default=1, help="number of loop unrolling (default: 1)")
  parser.add_argument ("-t", "--trx-len", type=int, default=1, help="maximum length of transaction scenario (default: 1)")
  args = parser.parse_args ()
  # Initialize env
  ## Add env for benchmark directory path and candidates
  env['benchmark'] = os.path.join (BENCHMARK_DIR, args.benchmark)
  if not (os.path.isdir (env['benchmark'])):
    raise Exception ("Wrong benchmark directory")
  else:
    env['candidates'] = sorted(os.listdir(env['benchmark']))
  ## Add env for output file path
  if not (os.path.isdir (OUTPUT_DIR)):
    os.mkdir(OUTPUT_DIR)
  if args.output == "":
    env['output'] = os.path.join (OUTPUT_DIR, args.benchmark)
  else:
    env['output'] = os.path.join (OUTPUT_DIR, args.output)
  env['output'] += str(datetime.now().strftime("_%Y%m%d%H%M%S.out"))
  ## Add env for time budget
  env['z3_time'] = args.z3_time_budget
  env['prover_time'] = args.prover_time_budget
  env['refuter_time'] = args.refuter_time_budget
  env['unroll_loop'] = args.unroll_loop
  env['trx_len'] = args.trx_len
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
  pool = multiprocessing.Pool(processes=(max(multiprocessing.cpu_count()-1, 1)))
  pool.starmap(run_f, idx_candidate_env_runrst_syncdata_list)
  # I don't know why, but close() and join() recommended at "https://stackoverflow.com/a/47683305"
  pool.close()
  pool.join()
  # Write Outputs
  with open (env['output'], 'w') as fp:
    for job in runrst:
      fp.write(job['resultLog'])
  return None


def run_f (idx, file, env, runrst, syncdata):
  # Set command line
  #cmd = ["dune", "exec", "--", "micse"]
  cmd = ["./_build/install/default/bin/micse"]
  cmd += ["-input", os.path.join (env['benchmark'], file)]
  cmd += ["-z3_timeout", str (env['z3_time'])]
  cmd += ["-prover_timeout", str (env['prover_time'])]
  cmd += ["-refuter_timeout_t", str (env['refuter_time'])]
  cmd += ["-unroll_l", str (env["unroll_loop"])]
  cmd += ["-unroll_t", str (env["trx_len"])]
  # Job Started
  job = dict()
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
    + "...[{}s - {}/{}]\n\n".format (job['runTime'], job['idx'], syncdata[1])
  )
  tempFP.close()
  sys.stderr.write ("  > Job #{} is done in {}s [{}/{}]\n".format (job['idx'], job['runTime'], syncdata[0], syncdata[1]))
  runrst.append(job)
  return None


if __name__ == "__main__":
  main ()