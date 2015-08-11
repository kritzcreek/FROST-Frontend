var gulp = require('gulp');
var webpack = require('webpack-stream');
var purescript = require('gulp-purescript');

var paths = {
  'psc': 'src/**/*.purs',
  'javascript': 'static/*js',
  'static': 'static/**/*',
  'pscLib': 'bower_components/*/src/**/*.purs'
};

gulp.task('copy-css', function() {
   gulp.src(['static/main.css',
             'bower_components/bootstrap/dist/css/bootstrap.min.css',
             'bower_components/eonasdan-bootstrap-datetimepicker/build/css/bootstrap-datetimepicker.min.css'])
   .pipe(gulp.dest('./dist/css'));
});

gulp.task('copy-fonts', function() {
   gulp.src('bower_components/bootstrap/fonts/*.{woff,woff2}')
   .pipe(gulp.dest('./dist/fonts'));
});

gulp.task('copy-index-html', function() {
    gulp.src('static/index.html')
    .pipe(gulp.dest('./dist'));
});

gulp.task('copy-bullshit', function() {
   gulp.src(['bower_components/jquery/dist/jquery.js',
             'bower_components/rxjs/dist/rx.lite.js',
             'bower_components/rxjs-jquery/rx.jquery.js',
             'bower_components/moment/moment.js',
             'bower_components/eonasdan-bootstrap-datetimepicker/build/js/bootstrap-datetimepicker.min.js'
            ])
   .pipe(gulp.dest('./dist/js/lib'));
});


var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

gulp.task("make", function () {
    return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("bundle", ["make"], function () {
    return purescript.pscBundle({ src: "output/**/*.js", output: "dist/bundle.js", main: 'Main' });
});

gulp.task('watch', function() {
  gulp.watch(paths.psc, ['default']);
  gulp.watch(paths.static, ['default']);
});

gulp.task('default', ['purescript', 'copy-index-html', 'copy-css',
          'copy-fonts', 'copy-bullshit', 'watch'], function() {
  return gulp.src('static/entry.js')
    .pipe(webpack(require('./webpack.config.js')))
    .pipe(gulp.dest('dist/'));
});
