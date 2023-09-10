# -*- mode: snippet -*-
#name : logging
#key : logging_
#contributor : Chong-Chong He
# --
DEBUG = 0
logging.basicConfig(level=logging.INFO)
Logger = logging.getLogger(__name__)
if DEBUG:
    Logger.setLevel(logging.DEBUG)