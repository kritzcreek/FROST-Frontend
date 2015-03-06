rm -r build/

mkdir build
mkdir build/js/
mkdir build/js/lib
mkdir build/css
mkdir build/fonts

#grunt
pulp browserify --source-map -O --main "Openspace.Ui.Stream" --to build/js/index.js

jsx static/ build/js/

#HTML
cp static/index.html build/index.html

#CSS
cp static/main.css build/css/main.css
cp bower_components/bootstrap/dist/css/bootstrap.min.css build/css/
cp bower_components/eonasdan-bootstrap-datetimepicker/build/css/bootstrap-datetimepicker.min.css build/css/


#FONTS
cp bower_components/bootstrap/fonts/glyphicons-halflings-regular.woff build/fonts/
cp bower_components/bootstrap/fonts/glyphicons-halflings-regular.woff2 build/fonts/

#JS
cp bower_components/react/react.js build/js/lib/react.js
cp bower_components/react-bootstrap/react-bootstrap.js build/js/lib/
cp bower_components/jquery/dist/jquery.js build/js/lib/
cp bower_components/rxjs/dist/rx.all.js build/js/lib/
cp bower_components/rxjs-jquery/rx.jquery.js build/js/lib/
cp bower_components/lodash/lodash.js build/js/lib/
cp bower_components/eonasdan-bootstrap-datetimepicker/build/js/bootstrap-datetimepicker.min.js build/js/lib/
cp bower_components/moment/moment.js build/js/lib/
