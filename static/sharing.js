function generateToken() {
    return (new Date().getTime()) +
	"#" +
	Math.round(999999 * Math.random());
}

$('form').append("<iframe name='blackhole' style='display: none'></iframe>");
$('form').attr('target', 'blackhole');
var token = generateToken();
$('form').attr('action', $('form').attr('action') + token);
$('form').submit(function() {
    var el = $('<p></p>');
    $('form').after(el);
    var desc = $("<div class='description'><h2>Add a description</h2><textarea rows='4' cols='40'></textarea><input type='submit' value='Add'></div>");
    desc.find('input').click(function() {
	$.ajax({ type: 'POST',
		 url: "/describe/" + token,
		 data: {
		     description: desc.find('textarea').val()
		 }
	       });
	desc.remove();
    });
    $(el).after(desc);
    $('form').hide();

    /* In case AJAX request comes before form submit, to retry with
       delay. Immedately set to 0 upon first success */
    var errorRetries = 10;

    function pollProgress() {
	console.log("poll");
	$.ajax({ url: "/progress/" + token,
		 dataType: 'json',
		 success: function(data) {
		     errorRetries = 0;

		     if (data.bytes && data.rate) {
			 el.text(data.bytes + " " + data.rate);
			 pollProgress();
		     } else if (data.link) {
			 el.text("Upload finished: ");
			 var link = $('<a></a>');
			 link.attr('href', data.link);
			 link.text(data.link);
			 el.append(link);

			 setTimeout(pollProgress, 10 * 1000);
		     }
		 },
		 error: function(e) {
		     console.error(e);
		     if (errorRetries) {
			 errorRetries--;
			 setTimeout(pollProgress, 500);
		     }
		 }
	       });
    }

    pollProgress();
});
