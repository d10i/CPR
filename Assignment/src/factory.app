{application, factory,
  [{vsn, "1.0.0"},
    {description, "Shopping cart system"},
    {modules, [address_verifier, cc, db, db_server, factory, request, requests_server, requests_sup, system_sup, util_sup, webclient, io]},
    {applications, [stdlib, kernel]},
    {registered, [address_verifier, cc, db_server, factory, requests_server, requests_sup, system_sup, util_sup]},
    {mod, {factory, []}}
  ]}.