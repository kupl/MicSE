import os
import sys
import time
import argparse
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
  return env

def run (env):
  jobLen = len(env.candidates)
  # Check make failure
  proc = subprocess.run ("make", stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True, cwd=PROJECT_DIR)
  if proc.returncode != 0:
    print (proc.stderr)
    return None
  # Run benchmarks
  with open (env.output, "w") as fp:
    for idx, file in enumerate (env.candidates):
      # Set command line
      cmd = ["dune", "exec", "--", "micse"]
      cmd += ["-input", os.path.join (env.benchmark, file)]
      # Job Started
      print ("  > Job [{}/{}] is started".format (idx + 1, jobLen))
      fp.write ("$ " + " ".join (cmd) + "\n")
      # Run Job
      timeStarted = time.time ()
      proc = subprocess.run (cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, cwd=PROJECT_DIR)
      timeFinished = time.time ()
      # Job Finished
      runTime = round ((timeFinished - timeStarted), 3)
      fp.write (proc.stdout)
      fp.write ("...[{}s]\n\n".format (runTime))
      print ("  > Job [{}/{}] is done in {}s".format (idx + 1, jobLen, runTime))
  return None

if __name__ == "__main__":
  main ()