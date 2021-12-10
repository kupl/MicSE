from micse_env import *
from slack_sdk import WebClient

class slack:
  def __init__(self):
    if not SLACK_POST:
      self.client = None
      self.body = None
    else:
      self.client = WebClient(token=SLACK_TOKEN)
      self.body = self.client.chat_postMessage(
        channel=SLACK_CHANNEL,
        text="> Script for the benchmark *`{}`* is started at _{}_.".format(
          INPUT_BENCHMARK,
          time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(SCRIPT_START_TIME))
        )
      )

  def post_result(self, job):
    if not SLACK_POST:
      return None
    else:
      with open(job['output'], "r") as fp:
        self.client.files_upload(
          channels=SLACK_CHANNEL,
          thread_ts=self.body['ts'],
          initial_comment="Job #{} for `{}`".format(job['idx'], job['target']),
          filename="{}_{}_{}.out".format(INPUT_BENCHMARK, job['idx'], time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(SCRIPT_START_TIME))),
          filetype="text",
          title="Result Log for `{}`".format(job['target']),
          content=fp.read()
        )
      return None
  
  def post_done(self):
    if not SLACK_POST:
      return None
    else:
      self.client.chat_update (
        channel=SLACK_CHANNEL,
        ts=self.body['ts'],
        text="""
          > Jobs for the benchmark *`{}`* is done at _{}_.\n
          ```\n
          Start Time: {}\n
          Elapsed Time: {}sec\n
          Options:\n
          \tMemory Budget: {}GB\n
          \tTotal Time Budget: {}sec\n
          \tZ3 Time Budget: {}sec\n
          \tPath Length: {}\n
          \tCore: {}\n
          ```
          """.format(
            INPUT_BENCHMARK,
            time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(SCRIPT_END_TIME)),
            time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(SCRIPT_START_TIME)),
            (SCRIPT_END_TIME - SCRIPT_START_TIME),
            TOTAL_MEMOUT,
            TOTAL_TIMEOUT,
            Z3_TIMEOUT,
            PATH_LIMIT,
            CORE
          ),
        )
      return None

# Genereate slack handler
SLACK = slack()