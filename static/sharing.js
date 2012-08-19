/**
 * Helpers
 */

function generateToken() {
    return (new Date().getTime()) +
	"#" +
	Math.round(999999 * Math.random());
}

function fileSize(input) {
    var total = 0;
    for(var i = 0; i < input.files.length; i++) {
	total += input.files[i].size;
    }
    return total;
}

function humanRate(rate) {
    var unit = "";
    var units = "KMGT";
    while(rate > 1024 && units) {
	rate /= 1024;
	unit = units.slice(0, 1);
	units = units.slice(1);
    }
    if (rate < 10)
	rate = Math.round(rate * 10) / 10;
    else
	rate = Math.round(rate);
    return rate + " " + unit + "B/s";
}

var form = $('form');

/**
 * Iframe to avoid page change
 */
form.append("<iframe name='blackhole' style='display: none'></iframe>");
form.attr('target', 'blackhole');

/**
 * Token for progress polling
 */
var token = generateToken();
form.attr('action', form.attr('action') + token);

/**
 * Auto Submit:
 */
$('form input:file').change(function() {
    $('form').submit();
});
$('form input:submit').hide();

/**
 * Submit handler: set description, update progress
 */
form.submit(function() {
    var progress = $('<p><progress></progress><span class="rate"></span></p>');
    progress.find('progress').prop('max', fileSize($('form input:file')[0]));
    form.after(progress);
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
    $(progress).after(desc);
    form.hide();

    /* In case AJAX request comes before form submit, to retry with
       delay. Immedately set to 0 upon first success */
    var errorRetries = 10;

    function pollProgress() {
	$.ajax({ url: "/progress/" + token,
		 dataType: 'json',
		 success: function(data) {
		     errorRetries = 0;

		     if (data.bytes && data.rate) {
			 progress.find('progress').prop('value', data.bytes);
			 progress.find('.rate').text(humanRate(data.rate));
			 pollProgress();
		     } else if (data.link) {
			 progress.empty();
			 progress.text("Upload finished: ");
			 var link = $('<a></a>');
			 link.attr('href', data.link);
			 link.text(data.link);
			 progress.append(link);

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
