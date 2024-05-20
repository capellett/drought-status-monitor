var url ='https://lakes.duke-energy.com/index.html#/detail/18/Detail#Detail';
var outfile = 'Wylie_level.html';
var page = new WebPage()
var fs = require('fs');

page.open(url, function (status) {
  page.evaluate(function() {
    test = document.querySelector("select");
    //test = document.querySelector("lar-lake-sort-dropdown");
    //return(document.getElementById("Detail-History").getElementsByTagName("table")[1];

  fs.write(outfile, test, 'w');
  //fs.write(outfile, page.content, 'w');
  //fs.write("test", test, 'w');
  phantom.exit();    
  });
});


  
//  //fs.write("test", document.querySelector("#lar-lake-sort-dropdown").value, 'w');
      
//      page.evaluate(function() {
//      const slct = document.querySelector('#lar-lake-sort-dropdown');
//        slct.options[2].selected = true;
//      }); 
      
 //     fs.write(outfile, page.content, 'w');
      
