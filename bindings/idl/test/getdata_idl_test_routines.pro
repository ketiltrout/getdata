pro timed_demo_mode
  print,""
  print,"**********************************************"
  print,"* The GetData IDL bindings cannot be tested in"
  print,"* timed demo mode.  All tests will be skipped."
  print,"**********************************************"
  print,""
  exit,status=77
end

function check_float, t, v, g
  if (abs(v - g) gt 1e-6) then begin
    print,"n[", t, "]=", v
    return,1
  endif

  return,0
end

function check_simple2, t, m, v, g
  if (total(v ne g)) then begin
    print,"n[", t, ",", m, "]=", v
    return,1
  endif

  return,0
end

function check_simple, t, v, g
  if (total(v ne g)) then begin
    print,"n[", t, "]=", v
    return,1
  endif

  return,0
end

function check_error, t, d, ce
  e = gd_error(d)
  if (e ne ce) then begin
    print,"e[", t, "]=", e
    return,1
  endif

  return,0
end

function check_ok, t, d
  return, check_error(t,d,!GD.E_OK)
end

function check_error2, t, m, d, ce
  e = gd_error(d)
  if (e ne ce) then begin
    print,"e[", t, ",", m, "]=", e
    return,1
  endif

  return,0
end

function check_ok2, t, m, d
  return, check_error2(t,m,d,!GD.E_OK)
end
