import os
import sys
import time
import argparse
import tempfile
import subprocess
from datetime import datetime

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.join (BASE_DIR, "..")
BENCHMARK_DIR = os.path.join (PROJECT_DIR, "benchmarks")
OUTPUT_DIR = os.path.join (BASE_DIR, "output")

class dotdict(dict):
    """dot.notation access to dictionary attributes"""
    __getattr__ = dict.get
    __setattr__ = dict.__setitem__
    __delattr__ = dict.__delitem__

def main ():
  try:
    sys.stderr.write ("> Start\n")
    env = read_env ()
    run (env)
    sys.stderr.write ("> Done\n")
  except Exception as ex:
    excType, excValue, excTrackback = sys.exc_info ()
    sys.stderr.write ("> Error: {}\n\n".format (ex))
    traceback.print_tb (excTrackback, file=sys.stderr)
  return None

def read_env ():
  env = dotdict()
  # Parse arguments
  parser = argparse.ArgumentParser ()
  parser.add_argument ("-b", "--benchmark", type=str, default="toy", help="benchmark directory which to examinate (default: toy)")
  parser.add_argument ("-o", "--output", type=str, default="", help="output file name (default: same with benchmark option)")
  parser.add_argument ("-z", "--z3-time-budget", type=int, default=30, help="time budget for Z3 solver in second (default: 30s)")
  parser.add_argument ("-p", "--prover-time-budget", type=int, default=180, help="time budget for whole prover process in second (default: 180s)")
  args = parser.parse_args ()
  # Initialize env
  ## Add env for benchmark directory path and candidates
  env.benchmark = os.path.join (BENCHMARK_DIR, args.benchmark)
  if not (os.path.isdir (env.benchmark)):
    raise Exception ("Wrong benchmark directory")
  else:
    env.candidates = os.listdir(env.benchmark)
  ## Add env for output file path
  if not (os.path.isdir (OUTPUT_DIR)):
    os.mkdir(OUTPUT_DIR)
  if args.output == "":
    env.output = os.path.join (OUTPUT_DIR, args.benchmark)
  else:
    env.output = os.path.join (OUTPUT_DIR, args.output)
  env.output += str(datetime.now().strftime("_%Y%m%d%H%M%S.out"))
  ## Add env for time budget
  env.z3_time = args.z3_time_budget
  env.prover_time = args.prover_time_budget
  return env

def run (env):
  jobLen = len(env.candidates)
  # Check make failure
  proc = subprocess.run ("make", stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True, cwd=PROJECT_DIR)
  if proc.returncode != 0:
    print (proc.stderr)
    return None
  # Run benchmarks
  pool = []
  for idx, file in enumerate (env.candidates):
    # Set command line
    cmd = ["dune", "exec", "--", "micse"]
    cmd += ["-input", os.path.join (env.benchmark, file)]
    cmd += ["-z3_timeout", str (env.z3_time)]
    cmd += ["-prover_timeout", str (env.prover_time)]
    # Job Started
    job = dotdict()
    job.idx = idx + 1
    sys.stderr.write ("  > Job #{} is started [{}/{}]\n".format (job.idx, job.idx, jobLen))
    job.tempFP = tempfile.TemporaryFile ()
    job.cmd = cmd
    job.startTime = time.time ()
    job.proc = subprocess.Popen (cmd, stdout=job.tempFP, stderr=subprocess.STDOUT, text=True, cwd=PROJECT_DIR)
    job.endTime = job.startTime
    job.runTime = 0.000
    job.terminateCheck = False
    # Print Log
    pool.append(job)
    time.sleep(0.5)
  # Write Outputs
  with open (env.output, "w") as fp:
    terminatedJobCount = 0
    while True:
      terminateCheck = True
      for job in pool:
        # Check Termination of Process
        if job.proc.poll () == None:
          # Skip the Non-terminated process
          terminateCheck = False
        else:
          if job.terminateCheck: continue
          else:
            terminatedJobCount += 1
            # Check Run Time
            job.endTime = time.time ()
            job.runTime = round ((job.endTime - job.startTime), 3)
            job.terminateCheck = True
            sys.stderr.write ("  > Job #{} is done in {}s [{}/{}]\n".format (job.idx, job.runTime, terminatedJobCount, jobLen))
      # Break the Wait Loop when All of Job is Terminated
      if terminateCheck: break
      # Wait the Next Termination Check Loop
      time.sleep (1)
    for job in pool:
      job.tempFP.seek (0)
      fp.write ("$ " + " ".join (job.cmd) + "\n")
      fp.write (job.tempFP.read ().decode ())
      fp.write ("...[{}s - {}/{}]\n\n".format (job.runTime, job.idx, jobLen))
      job.tempFP.close ()
  return None

if __name__ == "__main__":
  main ()