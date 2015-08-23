var gulp = require('gulp');
var webpack = require('webpack-stream');
var purescript = require('gulp-purescript');
var runSequence = require('run-sequence');
var del = require('del');

var paths = {
    'psc': ['src/**/*.purs', 'src/**/*.js'],
    'javascript': 'static/*js',
    'static': 'static/*',
    'deployFolder': '../FROST-Backend/static/'
};

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];

var foreigns = [
    'src/**/*.js',
    'bower_components/purescript-*/src/**/*.js'
];

gulp.task('clean', function(){
    return del([
        'dist/',
        'output/'
    ]);
});

gulp.task('copy-index-html', function() {
    return gulp.src('static/index.html')
        .pipe(gulp.dest('dist/'));
});

gulp.task('copy-css', function(){
    return gulp.src('static/main.css')
        .pipe(gulp.dest('dist/css/'));
});

gulp.task('copy', ['copy-index-html', 'copy-css']);

gulp.task('make', function () {
    return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task('bundle-psc', ['make'], function (cb) {
    return purescript.pscBundle({ src: 'output/**/*.js', output: 'dist/bundle.js', main: 'Main' });
});

gulp.task('bundle', function(){
     return gulp.src('static/entry.js')
        .pipe(webpack(require('./webpack.config.js')))
        .pipe(gulp.dest('dist/'));
});

gulp.task('deploy', function(){
    gulp.src(["dist/*", "dist/**/*"])
        .pipe(gulp.dest(paths.deployFolder));
});

gulp.task('watch', function() {
    gulp.watch(paths.psc, ['bundle-psc', 'bundle']);
    gulp.watch(paths.static, ['copy', 'bundle']);
});

gulp.task('default', function(cb){
    runSequence('clean', ['copy', 'bundle-psc'], 'bundle', 'watch', cb);
});
