---
layout: default
title: Rockets 2.0 Documentation - Customizing your Rockets Blog
---

# Customizing your Rockets Blog

To customize your Rockets blog, you must log in as the Admin user. See [Running your Rockets blog](running_rockets_blog) for details.

## Go to the Admin page

Once you have signed in as the Admin user, click your user name at the upper right of the top menu, then click "Admin page" from the dropdown menu.

The Admin page contains five sub-pages, identified by blue buttons/tabs on the top of the page. These include:

* General Configuration
* Custom Configuration
* Media Configuration
* User Configuration
* Podcast Configuration

## General Configuration

Here you can set custom images for your blog header and forum header, change the name of your blog, customize the top menu, and configure page layouts.

### Changing blog and header images

To change blog and header forum images, click "Choose File", select a file from your computer and click OK, then click "Upload".

**WARNING: There is no file size limit in this dialog box. Try to choose image files 800 x 600 pixels or smaller.**

### Customizing the menu

To customize the top menu, enter in the menu name and target page name in the box. You can also delete menu items individually, or add new blank ones.

**NOTE: The page name doesn't have to include the ".lsp" extension, unless it is a numbered page, such as "rockets-item.lsp?p=127"**

### Changing page layouts

You can change these layouts at any time.

The options for the Main Page layout are:

* Single page with custom content
* Single plage with blog posts (the default)
* Two columns with custom left hand navbar content
* Three columns with custom left and right hand navbar content

I like to have my main page set to a three-column layout, since that allows for all sorts of cool ways to promote other projects on your main page.

NOTE: If you choose "Single page with custom content", that custom content must be programmed by you. See [Extending Rockets](extending_rockets.md) for more information.

The options for individual page layout are:

* Imdividual post by itself
* Two columns with custom left hand navbar content
* Three columns with custom left and right hand navbar content

Below these options, you can select the custom content for the left and right hand navbars (columns).

The content options are:

* Custom HTML display box 1
* Most popular blog posts
* Recent forum posts
* Forum link
* Custom HTML display box 2
* Custom HTML display box 3
* Blog topics
* Custom HTML display box 4

You can select none, some, or all of these items, but you can't change their order. Yet.

The "Custom HTML display box" can literally display any HTML that you want. This is really useful for things like newsletters that offer custom signup HTML panels. 

You edit these custom HTML display boxes by clicking the "Custom Configuration" tab at the top of the page (see below)

## Custom configuration

These are the custom HTML display boxes described above.

You can put any raw HTML you want in here, including HTML that contains Javascript (usually using the <script> tag). However, any Javascript files referenced by a custom HTML snippet must actually exist somewhere on the web. This could include your own website, but you would need to manually upload them to your site and reference them by a valid URL.
  
Since these boxes can contain literally any HTML, you should be careful what you copy and paste into here, and make sure that only people you trust have access to the Admin account for your blog.

## Media configuration
  
This page contains all the images that you or your users have uploaded to your blog.  You can add new images, view images in full size by clicking on them, or delete images by clicking on the "Delete" button when the image is full-size.
  
## User configuration
  
This lists all registered users on your blog, and allows you to delete them if you want.
  
**NOTE: There is no confirmation on deleting users at the moment, so be careful. You cannot delete the Admin user, however.**
  
## Podcast configuration
  
This allows you to configure multiple podcasts on your site. You can configure the podcast name, description, email, image, and other tags that will be used when adding a new podcast post.
  
**NOTE: This section is experimental and may not work. Use with caution**.
