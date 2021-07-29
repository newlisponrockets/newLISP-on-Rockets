---
layout: default
title: Rockets 2.0 Documentation
---

![Rockets Logo](images/newlisp-rockets-picture-small.jpg)

# Table of contents
1. [Introduction](#introduction)
2. [Why Rockets?](#section1)
3. [Installing Rockets](#section2)
4. [Running your Rockets blog](#section3)
5. [Customizing your Rockets blog](#section4)
6. [How Rockets works](#section5)
7. [Extending Rockets](#section6)
8. [Developing for Rockets](#section7)
9. [The future of Rockets](#section8)
10. [Rockets API reference](#section9)

## Introduction <a name="introduction"></a>

Rockets is a fast, fun, and customizable blog application that is open source. It's also an easy-to-learn web application development framework.

It runs on the Linux platform.

## Why Rockets? <a name="section1"></a>

You may be interested in running Rockets if you:

* Want to experiment with a website that you can extend any way you like
* Are interested in LISP and LISP-like languages
* Feel like trying something fast, fun, and different

If you are looking for a full-featured blog application with tons of available themes and plugins, you probably want something like 
[WordPress](wordpress.org) instead.

## Installing Rockets <a name="section2"></a>

To install Rockets, you will need a Linux operating system. This can be either:

1. A version of Linux installed on your home computer as the main operating system
2. A version of Linux running on a virtual machine (such as VirtualBox)
3. A version of Linux installed on a cloud service provider, such as Amazon AWS or Linode

If you are just experimenting at home, the first two options are fine. If you want to run Rockets as a public website,
choose the third option. Most home Internet service providers don't allow you to run public websites from your home computer.

For step-by-step instructions, click one of the links below:

* [Installation guide for Rockets on Ubuntu 20.04](install_rockets_ubuntu.md) (recommended for experimentation)
* Installation guide using the Linode cloud provider (recommended for public websites)
* Manual installation guide for Rockets on non-Ubuntu Linux operating systems 

## Running your Rockets blog <a name="section3"></a>

Rockets works like most blogs. It displays a list of blog posts with the most recent displayed first.  However, Rockets has a few unique features. 

Specifically, Rockets contains a **blog** and a **forum**, but they are connected to a single user login and a single database.

All new blog posts appear in the forum under a different view, but only Admins can create new blog posts. Any registered user can create new forum threads, reply to blogs, or reply to other forum threads.

To learn more about running a Rockets blog, click here: [Running a Rockets blog](running_rockets_blog.md)

## Customizing your Rockets blog <a name="section4"></a>

Rockets is customizable out-of-the box using the built-in Admin menu.

Specifically, you can change the following things:

* Whether the main page contains a single column, two columns, or three columns, or custom content
* What information appears in each column, including custom HTML
* The page links (name and destinations) that appear on the top menu
* The main graphic that appears at the top of the site

For more information on this customization, click here: [Customizing your Rockets blog](customizing_rockets_blog.md)

For greater customizability, see the **Extending Rockets** section below.

## How Rockets works <a name="section5"></a>

Rockets is an application written in [newLISP](http://newlisp.org), a dialect of the LISP language.

The web server application [Apache](https://apache.org) takes pages that have the extension .lsp instead of the default .html, and redirects them to the newLISP interpreter. newLISP generates custom HTML for each page based on the code in that .lsp file.  The web server then sends the HTML to the user's web browser.

Data about users, blog posts, and forum posts is stored in a [SQLite](https://sqlite.org) database, running on the same server.

Configuration options for the blog are stored in text files with the .lisp extension, which are set to be non-viewable by the public.

For more information about how Rockets works, click here: [How Rockets works](how_rockets_works.md)

## Extending Rockets <a name="section6"></a>

Rockets is designed to be easily extendable. What does this mean? It's **extremely easy** to make a new page on your blog that behaves differently from all other pages. 

To learn more, click here: [Extending Rockets](extending_rockets.md)

## Developing for Rockets <a name="section7"></a>

Rockets is open source, which means that anyone can contribute ideas and code to the framework and blog application.

To view the code, and submit comments, change requests, or new code, click here:

[https://github.com/newlisponrockets/newLISP-on-Rockets](https://github.com/newlisponrockets/newLISP-on-Rockets)

## The future of Rockets <a name="section8"></a>

Rockets started out as way for me to learn how make custom websites. The blog application eventually grew to the point where I could replace my own personal blog at [jeremyreimer.com](https://jeremyreimer.com) with Rockets. 

This version became Rockets 2.0, which you are reading about now.

In the future, I want to work on Rockets 3.0, which will include new features that aren't standard in typical blogs. My vision is to create a kind of "virtual space" on the web that can grow and change with the owner and their own community. Like a home where you would invite guests and play fun little games, or design something new together.

This is a long-term project and I have no deadlines or milestones for Rockets 3.0 at the moment. Stay tuned!

## Rockets API reference <a name="section9"></a>

For a full and up-to-date Rockets application programming interface (API) reference, click here:

[Rockets API reference page](https://newlisponrockets.com/rockets-documentation.lsp)

