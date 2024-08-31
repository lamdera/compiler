function _Debug_toAnsiString(ansi, value)
{
  if (typeof value === 'function')
  {
    return _Debug_internalColor(ansi, '<function>');
  }

  if (typeof value === 'boolean')
  {
    return _Debug_ctorColor(ansi, value ? 'True' : 'False');
  }

  if (typeof value === 'number')
  {
    return _Debug_numberColor(ansi, value + '');
  }

  if (value instanceof String)
  {
    return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
  }

  if (typeof value === 'string')
  {
    return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
  }

  if (typeof value === 'object' && '$$' in value)
  {
    var tag = value.$$;

    if (typeof tag === 'number')
    {
      return _Debug_internalColor(ansi, '<internals>');
    }

    if (tag[0] === '#')
    {
      var output = [];
      for (var k in value)
      {
        if (k === '$$') continue;
        output.push(_Debug_toAnsiString(ansi, value[k]));
      }
      return '(' + output.join(',') + ')';
    }

    if (tag === 'Set_elm_builtin')
    {
      return _Debug_ctorColor(ansi, 'Set')
        + _Debug_fadeColor(ansi, '.fromList') + ' '
        + _Debug_toAnsiString(ansi, $$elm$$core$$Set$$toList(value));
    }

    if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
    {
      return _Debug_ctorColor(ansi, 'Dict')
        + _Debug_fadeColor(ansi, '.fromList') + ' '
        + _Debug_toAnsiString(ansi, $$elm$$core$$Dict$$toList(value));
    }

    if (tag === 'SeqSet_elm_builtin')
    {
      return _Debug_ctorColor(ansi, 'SeqSet')
        + _Debug_fadeColor(ansi, '.fromList') + ' '
        + _Debug_toAnsiString(ansi, $$lamdera$$containers$$SeqSet$$toList(value));
    }

    if (tag === 'SeqDict_elm_builtin')
    {
      return _Debug_ctorColor(ansi, 'SeqDict')
        + _Debug_fadeColor(ansi, '.fromList') + ' '
        + _Debug_toAnsiString(ansi, $$lamdera$$containers$$SeqDict$$toList(value));
    }

    if (tag === 'Array_elm_builtin')
    {
      return _Debug_ctorColor(ansi, 'Array')
        + _Debug_fadeColor(ansi, '.fromList') + ' '
        + _Debug_toAnsiString(ansi, $$elm$$core$$Array$$toList(value));
    }

    if (tag === '::' || tag === '[]')
    {
      var output = '[';

      value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

      for (; value.b; value = value.b) // WHILE_CONS
      {
        output += ',' + _Debug_toAnsiString(ansi, value.a);
      }
      return output + ']';
    }

    var output = '';
    for (var i in value)
    {
      if (i === '$$') continue;
      var str = _Debug_toAnsiString(ansi, value[i]);
      var c0 = str[0];
      var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
      output += ' ' + (parenless ? str : '(' + str + ')');
    }
    return _Debug_ctorColor(ansi, tag) + output;
  }

  if (typeof DataView === 'function' && value instanceof DataView)
  {
    return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
  }

  if (typeof File !== 'undefined' && value instanceof File)
  {
    return _Debug_internalColor(ansi, '<' + value.name + '>');
  }

  if (typeof value === 'object')
  {
    var output = [];
    for (var key in value)
    {
      var field = key[0] === '_' ? key.slice(1) : key;
      output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
    }
    if (output.length === 0)
    {
      return '{}';
    }
    return '{ ' + output.join(', ') + ' }';
  }

  return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
  var s = str
    .replace(/\\/g, '\\\\')
    .replace(/\n/g, '\\n')
    .replace(/\t/g, '\\t')
    .replace(/\r/g, '\\r')
    .replace(/\v/g, '\\v')
    .replace(/\0/g, '\\0');

  if (isChar)
  {
    return s.replace(/\'/g, '\\\'');
  }
  else
  {
    return s.replace(/\"/g, '\\"');
  }
}

function _Utils_eqHelp(x, y, depth, stack)
{
  if (x === y)
  {
    return true;
  }

  if (typeof x !== 'object' || x === null || y === null)
  {
    typeof x === 'function' && $$elm$$core$$Debug$$crash(5);
    return false;
  }

  if (depth > 100)
  {
    stack.push(_Utils_Tuple2(x,y));
    return true;
  }

  $equalsOverride

  for (var key in x)
  {
    if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
    {
      return false;
    }
  }
  return true;
}
