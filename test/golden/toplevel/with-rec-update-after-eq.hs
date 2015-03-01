dopt_set dfs f = dfs{ dumpFlags = IntSet.insert (fromEnum f) (dumpFlags dfs) }
