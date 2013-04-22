import os

def FlagsForFile( filename ):
  cmd = 'pygmalion --compile-flags ' + filename
  pygflags = os.popen(cmd).read().strip().split(' ')

  return {
    'flags': pygflags,
    'do_cache': True
  }
