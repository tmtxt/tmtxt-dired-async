# Asynchoronous execution library for Emacs Dired

A library for Emacs Dired mode to copy, compress, decompress files
asynchronously. It also provides the ability to mark files in multiple
directories and then copy all of them into a destination library.

# Usage

- Clone this repo and put it somewhere in your load-path
- Add this to your .emacs

{% highlight cl %}
(require 'tmtxt-dired-async)
{% endhighlight %}
