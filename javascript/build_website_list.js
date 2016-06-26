// scrape_techstars.js

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');

// Process passed paramter
var system = require('system');
var args = system.args;
// var modclothsite = "http://www.modcloth.com/shop/dresses#?sort=rating&page=1";
var path = "modclothDressList.html";
var modclothsite = args[1];
// var path = args[2];

page.open(modclothsite, function() {
  setTimeout(function() {
     var content = page.content;
     fs.write(path,content,'w')
     phantom.exit()
  }, 15000);
});