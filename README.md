# Tandem on Emacs

This repository contains code for the Emacs plugin for Tandem. For
more details on Tandem, visit [their
website](http://typeintandem.com/), or their [mono-repository
containing all other source
code.](https://github.com/typeintandem/tandem).

## Status

So far this is just an incomplete hack, don't use this for anything
serious yet.

## Installation

This is not yet hosted on MELPA.

For now, clone this repository, then build the CLRT client as follows:

    (cd tandem/crdt/ && yarn && yarn build)

Then load file `tandem.el`

## Usage

### Joining an Existing Session

    M-x tandem-join-session
    
You will be prompted for the session ID.

Note that you can enter any text as long as it contains a UUID.

### Hosting a Session

    M-x tandem-host-session

This hosts a session for the current buffer.

The session ID will be shown in the minibuffer and copied to the kill
ring.

### Getting the ID for an Active Session

    M-x tandem-show-session
    
This shows the session ID and copies it to the kill ring.

### Killing a Session

    M-x tandem-kill-session
    
This kills the session for the current buffer.

