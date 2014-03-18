{application, factory,
  [{vsn, "1.0.0"},
    {description, "Shopping cart system"},
    {modules, [address_verifier, cc, db, dist_db_server, db_server, factory, request, requests_server, requests_sup, factory_controller, system_sup, util_sup, webclient, io]},
    {applications, [stdlib, kernel]},
    {registered, [address_verifier, cc, dist_db_server, db_server, factory, requests_server, requests_sup, factory_controller, system_sup, util_sup]},
    {mod, {factory, ['node1@X201', 'node2@X201']}}
  ]}.