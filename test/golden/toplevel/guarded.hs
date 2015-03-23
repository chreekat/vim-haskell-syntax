defaultObjectTarget platform
  | platformUnregisterised platform     =  HscC
  | cGhcWithNativeCodeGen == "YES"      =  HscAsm
combineSafeFlags a b | a == Sf_None         = return b
getVerbFlags dflags
  | verbosity dflags >= 4 = ["-v"]
