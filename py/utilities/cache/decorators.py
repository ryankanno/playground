import inspect
import logging

from functools import wraps
from dogpile import cache_get, cache_set

logger = logging.getLogger(__name__)

def default_key_fun_impl(fun, *args, **kwargs):
    name      = fun.__name__
    mod       = fun.__module__
    call_args = inspect.getcallargs(fun, *args, **kwargs)

    return "%s-%s-%s" % (name, mod, '-'.join(["%s-%s" % (k, call_args[k]) 
        for k in sorted(call_args.iterkeys())]))


def cacheable(cache, key=None, ttl=60, enabled=True):
    """
    Decorator for cacheable function
    """
    def decorator(fxn):
        if callable(key):
            key_fun = key 
        else:
            key_fun = default_key_fun_impl if key is None else \
                lambda fxn, *args, **kwargs: key
        @wraps(fxn)
        def wrapper(*args, **kwargs):
            if enabled:
                key  = key_fun(fxn, *args, **kwargs)
                data = cache_get(cache, key)
                if data is None:
                    data = fxn(*args, **kwargs)
                    cache_set(cache, key, data, ttl)
                return data
            else:
                return fxn(*args, **kwargs)
        return wrapper
    return decorator 
