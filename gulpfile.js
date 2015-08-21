var gulp = require('gulp');
var webpack = require('webpack-stream');
var purescript = require('gulp-purescript');

var paths = {
    'psc': ['src/**/*.purs', 'src/**/*.js'],
    'javascript': 'static/*js',
    'static': 'static/*',
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
   gulp.src(['node_modules/jquery/dist/jquery.js',
             'node_modules/rx/dist/rx.lite.min.js',
             'node_modules/rx-jquery/rx.jquery.min.js',
             'node_modules/moment/moment.js',
             'bower_components/eonasdan-bootstrap-datetimepicker/build/js/bootstrap-datetimepicker.min.js'
            ])
   .pipe(gulp.dest('./dist/js/lib'));
});

gulp.task('copy', ['copy-css', 'copy-fonts', 'copy-index-html', 'copy-bullshit']);


var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

gulp.task("make", function () {
    return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("bundle-psc", ["make"], function (cb) {
    purescript.pscBundle({ src: "output/**/*.js", output: "dist/bundle.js", main: 'Main' });
    cb();
});

gulp.task('bundle', function(){
    return gulp.src('static/entry.js')
        .pipe(webpack(require('./webpack.config.js')))
        .pipe(gulp.dest('dist/'));
});

gulp.task('watch', function() {
    gulp.watch(paths.psc, ['bundle-psc', 'bundle']);
    gulp.watch(paths.static, ['copy', 'bundle']);
});

gulp.task('default', ['copy', 'bundle-psc', 'bundle', 'watch']);
