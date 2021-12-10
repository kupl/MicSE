import logging
import shutil
import sys
import traceback
from micse_env import *
from micse_run import *

# Main Function
def main():
  try:
    run_benchmarks()
  except Exception as ex:
    exc_type, exc_value, exc_trackback = sys.exc_info()
    shutil.rmtree(OUTPUT_DIR, ignore_errors=True)
    LOG.critical("{}".format(ex))
    traceback.print_tb(exc_trackback, file=sys.stderr)
  return None

if __name__ == "__main__":
  main ()