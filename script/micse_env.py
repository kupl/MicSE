import argparse
import logging
import multiprocessing
import os
import time
from dotenv import load_dotenv

# Global Settings
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.join(BASE_DIR, "..")
BENCHMARK_LOC_DIR = os.path.join(PROJECT_DIR, "benchmarks")
OUTPUT_LOC_DIR = os.path.join(PROJECT_DIR, "result")
BIN_LOC_DIR = os.path.join(PROJECT_DIR, "bin")
DOTENV_PATH = os.path.join(BASE_DIR, ".env")
BIN_LIST = ["micse", "micse.naive_trxpath_main", "micse.naive_refuter", "micse.naive_prover",
            "micse-s", "micse-s.enhanced_prover"]

# Environments Settings
load_dotenv (dotenv_path=DOTENV_PATH)

def get_env(name, default):
  try:
    return os.environ[name].strip(' "\'')
  except:
    return default

SLACK_TOKEN = get_env("SLACK_BOT_TOKEN", None)
SLACK_CHANNEL = get_env("SLACK_CHANNEL", None)

# Arguments Settings

## Time Settings
SCRIPT_START_TIME = time.time()
SCRIPT_END_TIME = time.time()

## Parse Arguments
parser = argparse.ArgumentParser()

### Argument List
parser.add_argument("-B", "--bin", type=str, required=True, help="binary file name for running (REQUIRED)")
parser.add_argument("-I", "--input", type=str, required=True, help="input benchmark name for running scripts (REQUIRED)")
parser.add_argument("-O", "--output", type=str, default=time.strftime("%Y%m%d%H%M%S", time.gmtime(SCRIPT_START_TIME)), help="directory name for storing the results (Default: YYYYmmddHHMMSS)")
parser.add_argument("-M", "--memory-bound", type=int, default=5, help="memory budget for entire MicSE in GB (default: 5)")
parser.add_argument("-T", "--total-timeout", type=int, default=360, help="time budget for entire MicSE execution in seconds. (default: 360)")
parser.add_argument("-Z", "--z3-timeout", type=int, default=30, help="time budget for Z3 solver in seconds (default: 30)")
parser.add_argument("-P", "--path-limit", type=int, default=5, help="limit of length for path in single transaction (default: 5)")
parser.add_argument("-C", "--core", type=int, default=1, help="number of core to process benchmarks (default: 1)")
parser.add_argument("-MD", "--micse-debug", dest="micse_debug", action="store_true", help="set logging level for MicSE to debug level")
parser.add_argument("-MV", "--micse-verbose", dest="micse_verbose", action="store_true", help="set logging level for MicSE to info level")
parser.add_argument("-s", "--slack-post", dest="slack_post", action="store_true", help="flag for posting the result to slack (default: false)")
parser.add_argument("-d", "--debug", dest="debug", action="store_true", help="set logging level for script to debug level (default: false)")

### Set Default Value
parser.set_defaults(slack_post=False, micse_debug=False, micse_verbose=False, debug=False, verbose=False)

### Parse the Arguments
args = parser.parse_args()

## Set Arguments
BIN_NAME = args.bin
INPUT_BENCHMARK = args.input
OUTPUT_PREFIX = args.output
BENCHMARK_DIR = os.path.join(BENCHMARK_LOC_DIR, INPUT_BENCHMARK)
OUTPUT_DIR = os.path.join(OUTPUT_LOC_DIR, OUTPUT_PREFIX)
TOTAL_MEMOUT = args.memory_bound
TOTAL_TIMEOUT = args.total_timeout
Z3_TIMEOUT = args.z3_timeout
PATH_LIMIT = args.path_limit
CORE = max(1, min(args.core, multiprocessing.cpu_count() - 1))
SLACK_POST = args.slack_post
MICSE_LOG_LEVEL = "-d" if args.micse_debug else "-v" if args.micse_verbose else None

## Logger Settings
LOG = logging.getLogger()
if args.debug:
  LOG.setLevel(logging.DEBUG)
else:
  LOG.setLevel(logging.INFO)
LOG_FORMATTER = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
LOG_HANDLER = logging.StreamHandler()
LOG_HANDLER.setFormatter(LOG_FORMATTER)
LOG.addHandler(LOG_HANDLER)
LOG.info("Logger start")

## Arguments Validation
if BIN_NAME not in BIN_LIST:
  LOG.critical("Wrong binary file name: {}".format(BIN_NAME))
  raise Exception("ARG_VALIDATE_ERR")
if not (os.path.isdir(BENCHMARK_LOC_DIR)):
  LOG.critical("Wrong benchmark located directory path: {}".format(BENCHMARK_LOC_DIR))
  raise Exception("ARG_VALIDATE_ERR")
if not (os.path.isdir(OUTPUT_LOC_DIR)):
  os.mkdir(OUTPUT_LOC_DIR)
  LOG.critical("Wrong output located directory path: {}".format(OUTPUT_LOC_DIR))
  raise Exception("ARG_VALIDATE_ERR")
if not (os.path.isdir(BENCHMARK_DIR)):
  LOG.critical("Wrong benchmark directory path: {}".format(BENCHMARK_DIR))
  raise Exception("ARG_VALIDATE_ERR")
if not (os.path.isdir(OUTPUT_DIR)):
  os.mkdir(OUTPUT_DIR)
else:
  LOG.critical("Wrong output directory path (already exists): {}".format(OUTPUT_LOC_DIR))
  raise Exception("ARG_VALIDATE_ERR")
if not (MICSE_LOG_LEVEL == "-d" or MICSE_LOG_LEVEL == "-v" or MICSE_LOG_LEVEL == None):
  LOG.critical("Wrong log level: {}".format(MICSE_LOG_LEVEL))
  raise Exception("ARG_VALIDATE_ERR")
