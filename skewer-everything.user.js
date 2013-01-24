// ==UserScript==
// @name         Skewer Everything
// @description  Run Skewer on everything
// @lastupdated  2013-01-24
// @version      1.0
// @license      Public Domain
// @include      http://*
// @require      http://code.jquery.com/jquery-latest.min.js
// ==/UserScript==

var script = $('<script/>').attr({src: 'http://localhost:8080/skewer'});
if (!unsafeWindow.jQuery) unsafeWindow.$ = $;
$('head').append(script);
