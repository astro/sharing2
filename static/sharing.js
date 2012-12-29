/**
 * Helpers
 */

function generateToken() {
    return (new Date().getTime()) +
	"-" +
	Math.round(999999 * Math.random());
}

function fileSize(input) {
    var total = 0;
    if (input.files && input.files.length)
        for(var i = 0; i < input.files.length; i++) {
            var f = input.files[i];
	    total += f.fileSize || f.size;
        }
    else if (window.ActiveXObject) {
        try {
            var fso = new ActiveXObject("Scripting.FileSystemObject");
            var f = fso.getFile(input.value);
            if (f)
                total = f.size;
        } catch (e) {
        }
    }
    return total;
}

function humanSize(rate) {
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
    return rate + " " + unit + "B";
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
form.attr('action', form.attr('action').replace(/-$/, "") + token);

/**
 * Auto Submit:
 */
$('form input:file').change(function() {
    form.submit();
});
$('form input:submit').hide();

/**
 * Submit handler: set description, update progress
 */
form.submit(function() {
    var progress = $('<p><span class="percent"></span><progress></progress><span class="rate"></span></p>');
    var totalBytes = fileSize($('form input:file')[0]);
    progress.find('progress').attr('max', totalBytes);
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
                         if (totalBytes)
			     progress.find('.percent').text(
			         Math.floor(100 * data.bytes / totalBytes) + "%"
			     );
                         else
                             progress.find('.percent').text(
                                 humanSize(data.bytes)
                             );
			 progress.find('progress').attr('value', data.bytes);
			 progress.find('.rate').text(humanSize(data.rate) + "/s");
			 setTimeout(pollProgress(), 1);
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
                     if (window.console)
		         console.error(e && e.stack || e);
		     if (errorRetries) {
			 errorRetries--;
			 setTimeout(pollProgress, 500);
		     }
		 }
	       });
    }

    pollProgress();
});
