import multiprocessing
import os
import subprocess
import tempfile
from micse_env import *
from micse_post import *

# Run benchmarks
def run_benchmarks():
  collect_files()
  build_bin()
  run_multi()
  return None

# Collect the benchmark files
TARGET_LIST = list()

def collect_files():
  global TARGET_LIST
  filelist = os.listdir(BENCHMARK_DIR)
  tzlist = list(filter(lambda x: x.endswith(".tz") and not x.endswith(".storage.tz"), filelist))
  TARGET_LIST = list(map(lambda x: os.path.splitext(x)[0], tzlist))
  return None

# Build MicSE binary file
def build_bin():
  cmd = list()
  cmd += ["make"]
  proc = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True, cwd=PROJECT_DIR)
  if proc.returncode != 0:
    LOG.critical("Build Error\n{}".format(proc.stderr))
    raise Exception("Build Failed")
  return None
  
# Run benchmarks with parallely
def run_multi():
  global SCRIPT_END_TIME
  # Set manager for multiprocessing pool
  job_length = len(TARGET_LIST)
  manager = multiprocessing.Manager()
  res = manager.list([])
  syncdata = manager.list([0,job_length])
  # Make a multiprocessing pool
  raw_job = [(idx, target, res, syncdata) for idx, target in enumerate(TARGET_LIST)]
  pool = multiprocessing.Pool(processes=CORE)
  pool.starmap(run_each, raw_job)
  # Close and join the pool
  pool.close()
  pool.join()
  # Finalize the result
  SCRIPT_END_TIME = time.time()
  res = sorted(res, key=lambda job: job['idx'])
  SLACK.post_done()
  return None

# Run each targets in single process
def run_each(idx, target, res, syncdata):
  job = dict()
  # Generate tempfile that output will be writed
  job['output'] = "{}/{}_{}.out".format(OUTPUT_DIR, OUTPUT_PREFIX, target)
  LOG.info("job #{} is started. (tmpfile: {})".format(idx, job['output']))
  # Setting new job
  job['idx'] = idx
  job['target'] = target
  job['cmd'] = generate_command(target)
  job['start_time'] = time.time()
  # Launch the process
  tmp_fp = tempfile.TemporaryFile()
  subprocess.run(job['cmd'], stdout=tmp_fp, stderr=subprocess.STDOUT, text=True, cwd=PROJECT_DIR, timeout=TOTAL_TIMEOUT*1.1)
  # Finalize job
  job['end_time'] = time.time()
  job['run_time'] = round(job['end_time'] - job['start_time'], 3)
  job['terminate_check'] = True
  syncdata[0] += 1
  # Log the result
  with open(job['output'], "w") as fp:
    tmp_fp.seek(0)
    fp.write("$ {}".format(str.join(" ", job['cmd'])))
    fp.write("{}".format(tmp_fp.read().decode()))
    fp.write("...{}s [{}/{}]".format(job['run_time'], syncdata[0], syncdata[1]))
  LOG.info("job #{} is done in {}s [{}/{}].".format(idx, job['run_time'], syncdata[0], syncdata[1]))
  res.append(job)
  SLACK.post_result(job)
  return None

# Generate command line
def generate_command(target):
  cmd = list()
  cmd += ["{}/{}".format(BIN_LOC_DIR, BIN_NAME)]
  cmd += ["-I", "{}/{}.tz".format(BENCHMARK_DIR, target)]
  cmd += ["-S", "{}/{}.storage.tz".format(BENCHMARK_DIR, target)]
  cmd += ["-M", str(TOTAL_MEMOUT)]
  cmd += ["-T", str(TOTAL_TIMEOUT)]
  cmd += ["-Z", str(Z3_TIMEOUT)]
  cmd += ["-P", str(PATH_LIMIT)]
  if not MICSE_LOG_LEVEL == None: cmd += [MICSE_LOG_LEVEL]
  return cmd