Butler -- Emacs Integration for Jenkins (And maybe Hudson)
======

Install
=======

Add butler.el to your load path. This will be distributed to Marmalade in the future (it's on the list).

Dependencies
------------

* json.el
    * Should be distributed with Emacs.
* web.el
    * Available in Marmalade.

Getting Started
===============

Butler contains a list of servers conveniently named butler-servers. Add servers in the following manor:

```elisp
(add-to-list butler-servers
             '(jenkins "SERVER-NAME"
                       (server-address . "https://jenkins-addres")
                       (server-user . "user")
                       (server-password . "pass")))
```

In the future, the first variable will indicate what type of CI server is being used. Currently it's ignored.

View the list of servers with `M-x butler-status`. Refresh the list with `g`, and trigger the job under the point with `t`.

Coming Soon
===========

* EPA support.
* Indication of what jobs are running
* Console output.
* Job history.
* Job watching.
