chrome.extension.onRequest.addListener(
  function(request, sender, sendResponse) {
    urls = [];
    if (request.all) 
    {
      jQuery(request.all.join()).each(function() {
        var url = jQuery(this).attr('href');
        if (url && '' != url)
          urls.push(url);
      });
    }
    else if (request.unread)
    {
      var visited = null;
      for (var request in request.unread)
      {
        var request_visited = request + ":visited";
        if (null == visited)
        {
          visited = jQuery(request_visited);
        }
        else
        {
          visited.add(jQuery(request_visited));
        }
      }

      jQuery(request.unread.join()).filter(function(index) {
        if (visited.find(jQuery(this)).size() == 0)
        {
          var url = jQuery(this).attr('href');
          if (url && '' != url)
            urls.push(url);
        }
      });
    }
    sendResponse({urls: urls});
  }
);
