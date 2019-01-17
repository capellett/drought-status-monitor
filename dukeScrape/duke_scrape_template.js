var url ='https://lakes.duke-energy.com/index.html#/detail/18/Detail#Detail';
var outfile = 'Wylie_level.html';
var page = new WebPage()
var fs = require('fs');


page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write(outfile, page.content, 'w');
            phantom.exit();
    }, 2500);
}
