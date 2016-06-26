// scrape_techstars.js

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');

// Process passed paramter
var system = require('system');
var args = system.args;
var modclothsite = args[1];
// var path = "modcloth.html";
var path = args[2];
var timeRun = args[3];

page.open(modclothsite, function() {
  page.includeJs("http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js", function() {
    page.evaluate(function() {
      $("#viewall").click();
    });
    setTimeout(function() {
      var content = page.content;
      fs.write(path,content,'w')
      phantom.exit()
    }, Number(timeRun));

  });
});