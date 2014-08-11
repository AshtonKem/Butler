Butler -- Emacs Integration for Jenkins (And maybe Hudson)
======

Install
=======

Butler is available on [MELPA](http://melpa.milkbox.net/#/butler) & [MELPA-STABLE](http://melpa.milkbox.net/#/butler). Because of persistent Marmalade problems, I'm currently recommending either of the Melpa versions.

You will need to add `(require 'butler)`  to one of your init files.

Dependencies
------------

* json.el
    * Should be distributed with Emacs.
* deferred.el
    * Available in Marmalade.

Thanks
------
Even the smallest open source project is a team effort. The following people have helped Butler along:

* killdash9
    * For creating the configurable auto-refresh system

Getting Started
===============

Butler contains a list of servers conveniently named butler-servers. Add servers in the following manner:

```elisp
(add-to-list 'butler-server-list
             '(jenkins "SERVER-NAME"
                       (server-address . "https://jenkins-address")
                       (server-user . "user")
                       (server-password . "pass")))
```

Butler also supports putting credentials in an encrypted authinfo file, like gnus.

Use the following code to set that up:

```elisp
(add-to-list 'butler-server-list
             '(jenkins "SERVER-NAME"
                       (server-address . "https://jenkins-addres")
                       (auth-file . "~/.authinfo.gpg")))
```

The following line should exist in ~/.authinfo.gpg:

```
machine SERVER-NAME login username password password
```


In the future, the first variable will indicate what type of CI server is being used. Currently it's ignored.

View the list of servers with `M-x butler-status`. Refresh the list with `g`, and trigger the job under the point with `t`.

But I use SSO and Passwords Don't Work!
---------------------------------------

No worries. Jenkins provides an "API" token that can be used like a password, but only for Jenkins. To get your API token follow these steps:

* Log into Jenkins
* Click your name in the upper right corner
* Click "Configure" on the left side.
* Select "Show API Token"
* Copy token into .authinfo.gpg, or into your config files


Release Notes
=============

* 0.2.1
   * Configurable auto refresh
* 0.2.0
   * Customizable faces for the status dots.
   * Replaced web.el with deffered.el for better performance

Coming Soon
===========

* Console output.
* Job history.
* Job watching.


FAQ
===

1. Why?
    * Because I missed Jenkins integration when I left Eclipse for Emacs (again).
2. Why does it display an error at the bottom when I refresh?
    * web.el is being tricky. I might replace it with the native url.el package.
3. What if I want to use a Jenkins view instead of seeing everything?
    * Server-address can be either the base url for your server, or the url for the view.
4. Can I have multiple servers?
    * There's a bug affecting the rendering of multiple servers at the moment. I'm working on it.
5. Does it support Hudson/Travis/my-favorite-CI?
    * I've only tested with Jenkins at the moment. Since Jenkins and Hudson are so closely related, theoretically it should be fine. Please contact me if Hudson doesn't work.
    * I currently do not support other server types, sorry.
6. Do you support anything other than basic auth?
    * Kind of. Jenkins provides single user API tokens that can be used as a password.
