from __future__ import print_function
try:
  import pygame
  from pygame.locals import *
except:
  pass
import sys
import os
import time
import readline
import rlcompleter
# press tab to autocomplete!
readline.parse_and_bind("tab: complete")

from math import *
try:
  from see import *
except:
  pass

try:
  from git_repos.algorithms import *
except:
  pass

sys.ps1 = '>>> ' # normal >>> 
sys.ps2 = '... ' # normal ... 
sys.ps1 = '' # normal >>> 
sys.ps2 = '' # normal ... 
true = True
false = False

def clipboard(text=None):
  '''
  Given no argument, returns the contents of the system clipboard or
  None if empty.
  Given an argument string, sets that as the value of the system clipboard and
  returns the given string.

  Note: The presence of calling this function will result in the system
  clipboard being flushed on program termination.
  '''
  try:
    _ = clipboard.win
  except AttributeError:
    import Tkinter
    win = Tkinter.Tk()
    win.withdraw()
    clipboard.win = win
    clipboard.TclError = Tkinter._tkinter.TclError
  if text is None:
    try:
      return clipboard.win.selection_get(selection='CLIPBOARD')
    except clipboard.TclError:
      return None
  else:
    clipboard.win.clipboard_clear()
    clipboard.win.clipboard_append(text)
    return text

def interactive():
  import code
  # which way is better?
  #code.InteractiveConsole(locals=globals()).interact()
  code.interact(local=globals())


def pretty_int(n): # unexecuted version also works for float
  return '{:,}'.format(n)
  if n < 0:
    result = '-' + pretty_int(-n)
  result = ''
  while n >= 1000:
    n, r = divmod(n, 1000)
    result = ',%03d%s' % (r, result)
  return '%d%s' % (n, result)


'''def fact(n):
  if n == 0: return 1
  return reduce(lambda x,y: x*y, xrange(1,n+1))
'''
def fact(n):
  if n == 0: return 1
  i = 1
  while n > 0:
    i *= n
    n -= 1
  return i

def ncr(n, r):
  return fact(n) / (fact(r) * fact(n-r))

def npr(n, r):
  return fact(n) / fact(n-r)

def timeit(fn):
  def innerfun(*args, **kwargs):
    s = time.time()
    r = fn(*args, **kwargs)
    e = time.time()
    print("Elapsed time for %s: %s" %(fn.__name__, str(e - s)))
    return r
  innerfun.__name__ = fn.__name__
  return innerfun

def log2(x): return log(x) / log(2)

def odds(p): return p / (1-p)
def logit(x): return log2(odds(x))
def belief(x): return logit(x)
def expit(x): return 2**x/(1.+2**x)

# Euclid's algo for greatest common factor/denominator
def gcd(a,b):
  if b == 0: return a
  else: return gcd(b, a % b)

def benchmark(func):
    """
    A decorator that print the time of function take
    to execute.
    """
    import time
    def wrapper(*args, **kwargs):
        t = time.clock()
        res = func(*args, **kwargs)
        print(func.__name__ + ' ' + str(time.clock()-t))
        return res
    return wrapper

def counter(func):
    """
    A decorator that print the number of time a function has been executed
    """
    counter.count = 0
    def wrapper(*args, **kwargs):
        counter.count = counter.count + 1
        res = func(*args, **kwargs)
        print(func.__name__ + " has been used : " + str(counter.count) + " X")
        return res
    return wrapper

@counter
@benchmark
def reverse_string(string):
    return string[::-1]


def mymemo(fn): # yay memoization
  cache = {}
  def wrapper(*args, **kwargs):
    if (args,str(kwargs)) not in cache:
      cache[args,str(kwargs)] = fn(*args, **kwargs)
    return cache[args,str(kwargs)]
  return wrapper

def mymemo_env(init={}): # the extra wrapper on top of normal is for dec args
  # e.g. @mymemo_envize({ ( (0,) , str({}) ): 0 })
  def memo(fn):
    cache = init
    def wrapper(*args, **kwargs):
      if (args,str(kwargs)) not in cache:
        cache[args,str(kwargs)] = fn(*args, **kwargs)
      return cache[args,str(kwargs)]
    return wrapper
  return memo

def pygraphit():
  import pycallgraph
  pycallgraph.start_trace()
  pycallgraph.stop_trace()
  pycallgraph.make_graph('/tmp/bleh.png')

# this from active state recipe
class TailRecurseException:
  def __init__(self, args, kwargs):
    self.args = args
    self.kwargs = kwargs

def tail_call_optimized(g):
  """
  This function decorates a function with tail call
  optimization. It does this by throwing an exception
  if it is it's own grandparent, and catching such
  exceptions to fake the tail call optimization.
  
  This function fails if the decorated
  function recurses in a non-tail context.
  """
  def func(*args, **kwargs):
    f = sys._getframe()
    if f.f_back and f.f_back.f_back \
        and f.f_back.f_back.f_code == f.f_code:
      raise TailRecurseException(args, kwargs)
    else:
      while 1:
        try:
          return g(*args, **kwargs)
        except TailRecurseException, e:
          args = e.args
          kwargs = e.kwargs
  func.__doc__ = g.__doc__
  return func

'''
examples:
@tail_call_optimized
def factorial(n, acc=1):
  "calculate a factorial"
  if n == 0:
    return acc
  return factorial(n-1, n*acc)

print factorial(10000)
# prints a big, big number,
# but doesn't hit the recursion limit.

@tail_call_optimized
def fib(i, current = 0, next = 1):
  if i == 0:
    return current
  else:
    return fib(i - 1, next, current + next)

print fib(10000)
# also big num, no recursion limit
'''

def make_let():
  import contextlib
  @contextlib.contextmanager
  def let(*values):
    yield values
  return let
# usage:
# with let(1, 2, 3) as (a, b, c):
#   print(a,b,c)

print('Ready for action!')
