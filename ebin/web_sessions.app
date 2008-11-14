%% This is the application resource file (.app file) for the web_sessions,
%% application.
{application, web_sessions,
  [{description, "Sessions application to be used with web_router"},
   {vsn, "0.1.0"},
   {modules, [web_session, web_sessions, web_session_clone]},
   {registered, []},
   {applications, [kernel, stdlib, web_router, inets]}
  ]}.
