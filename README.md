# Erlang, Ruby, Webmachine Demo

This is a research spike I did at work. It is a RESTful web service built using
[Erlang](http://www.erlang.org/) [Webmachine](https://github.com/basho/webmachine)
for the infrastructure and [Ruby](http://www.ruby-lang.org/en/)
via [Erlectricity](https://github.com/mojombo/erlectricity) for the logic.
It can run locally using [Foreman](https://github.com/ddollar/foreman)
or be deployed to [Heroku](http://www.heroku.com/).

## How it works

The main application defines two routes. The root route is a simple
"hello world" page. The real magic occurs on the */hello* route. This
route, defined in *src/hello_resource.erl*, spawns a worker process that
dispatches requests to the Ruby file *lib/hello_resource.rb*. The
Ruby worker then uses the [Faker](http://faker.rubyforge.org/) gem
to generate a unique message for every request. The Ruby interpreter
is loaded only once and remains memory-resident until explicitly
shutdown by the Erlang VM.

### Running the app

First, make sure you have both Erlang and Ruby setup on your machine
and make sure both are in your path. Once you have those prerequsites
met do this:

Build the Erlang project with make

    make

Install [Bundler](http://gembundler.com/) to manage the Ruby gem
dependencies

    gem install bundler

Then install the required gems

    bundle install

Finally, run the app

    foreman start

Depending on you machine's setup the app will usually run on port
5000. To verify check the console output for somthing like this

    {webmachine_mochiweb,start,
      [[{ip,"0.0.0.0"},
        {port,"5000"},
        {dispatch,
          [{[],vivr_resource,[]},
           {["hello"],hello_resource,[]}]}]]}},
    
Open your browser to *http://localhost:5000/hello* (or the appropriate port)
and you should be greeted with a message like this:

    <<"Programmable multimedia forecast to extend integrated ROI.">>

## Deploying to Heroku

This app can be successfully deployed to and run on Heroku but it
requires a few shenanigans to make it work. The problem and solution
both concern Heroku's [buildpacks](https://devcenter.heroku.com/articles/buildpacks).
By default Heroku expects one and only one buildpack per application.
Making this app work on Heroku requires both the
[Erlang buildpack](https://github.com/heroku/heroku-buildpack-erlang) and the
[Ruby buildpack](https://github.com/heroku/heroku-buildpack-ruby).
The secret sauce in this case is the
[multi-language buildpack](https://github.com/ddollar/heroku-buildpack-multi).
The multi-language buildpack uses a *.buildpacks* file to list all
the buildpacks that need to be installed. The only problem is that the
path on the Heroku instance does not always get setup properly for
both Ruby and Erlang. This is an easy fix.

First, install the [Heroku toolbelt](https://toolbelt.heroku.com/)
and create a Heroku application following the
[normal creation process](https://devcenter.heroku.com/articles/cedar)
and add the required Git remote. Deploy the app and verify that it is
running by navigating to the root URL of the new app. The root URL
does not use Ruby and will work even if Ruby isn't setup properly.
Check if Ruby is in the path using the Heroku *run* command:

    heroku run ruby -v 
  
If you see something like this you are golden

    Running `ruby -v` attached to terminal... up, run.1830
    ruby 1.9.2p290 (2011-07-09 revision 32553) [x86_64-linux]

If you see somthing like this you'll need to modify your path

    Running `ruby -v` attached to terminal... up, run.2160
    bash: ruby: command not found

Search the Heroku instance for the Ruby interpreter

    heroku run find / -name ruby

You should see something like this
    
    Running `find / -name ruby` attached to terminal... up, run.8362
    find: `/proc/tty/driver': Permission denied
    find: `/proc/1/task/1/fd': Permission denied
    find: `/proc/1/task/1/fdinfo': Permission denied
    find: `/proc/1/fd': Permission denied
    find: `/proc/1/fdinfo': Permission denied
    /usr/local/include/ruby-1.9.1/x86_64-linux/ruby
    /usr/local/include/ruby-1.9.1/ruby
    /usr/local/share/doc/ruby
    /usr/local/bin/ruby
    /usr/local/lib/ruby
    find: `/lost+found': Permission denied
    find: `/etc/ssl/private': Permission denied
    /app/vendor/bundle/ruby
    
In this case the Ruby interpreter was found at */usr/local/bin/ruby*

Next, get a listing of the environment variables set on the Heroku instance

    heroku run printenv

Append the location of the Ruby executable to the Heroku path using the
Heroku config command using something like this:

    heroku config:set PATH=/app/bin:/app/vendor/bundle/ruby/1.9.1/bin:bin:/usr/bin:/bin:/usr/local/lib/erlang/bin:/usr/local/bin

Verify the configuration change by checking the environment variables
and by checking the new Heroku config

    heroku config
    
You should see something like this

    === heroku-app-1234 Config Vars
    BUILDPACK_URL: https://github.com/ddollar/heroku-buildpack-multi.git
    PATH:          /app/bin:/app/vendor/bundle/ruby/1.9.1/bin:bin:/usr/bin:/bin:/usr/local/lib/erlang/bin:/usr/local/bin

At this point everything should be good-to-go. Navigate the the
*/hello* route on the Heroku app and get your message.

## Useful links

* http://www.erlang.org/
* http://www.ruby-lang.org/en/
* https://github.com/basho/webmachine/wiki


* http://zianet.dk/blog/2011/12/16/running-erlang-webmachine-on-heroku/


* https://github.com/basho/webmachine/wiki
* https://github.com/mojombo/erlectricity


* https://github.com/ddollar/heroku-buildpack-multi
* https://github.com/heroku/heroku-buildpack-erlang
* https://github.com/heroku/heroku-buildpack-ruby


* https://devcenter.heroku.com/articles/creating-apps
* https://devcenter.heroku.com/articles/quickstart


* https://devcenter.heroku.com/articles/buildpacks
* https://bugsplat.info/2012-11-05-introduction-to-heroku-buildpacks.html

## Copyright

*Ratistics* is Copyright &copy; 2013 [Jerry D'Antonio](https://twitter.com/jerrydantonio).
It is free software and may be redistributed under the terms specified in
the LICENSE file.

## License

Released under the MIT license.

http://www.opensource.org/licenses/mit-license.php  

> Permission is hereby granted, free of charge, to any person obtaining a copy  
> of this software and associated documentation files (the "Software"), to deal  
> in the Software without restriction, including without limitation the rights  
> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell  
> copies of the Software, and to permit persons to whom the Software is  
> furnished to do so, subject to the following conditions:  
> 
> The above copyright notice and this permission notice shall be included in  
> all copies or substantial portions of the Software.  
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  
> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  
> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  
> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  
> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN  
> THE SOFTWARE.  
