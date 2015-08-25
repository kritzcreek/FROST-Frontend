var gulp = require('gulp');
var webpack = require('webpack-stream');
var purescript = require('gulp-purescript');
var sass = require('gulp-sass');
var minifyCss = require('gulp-minify-css');
var autoprefixer = require('gulp-autoprefixer');
var runSequence = require('run-sequence');
var del = require('del');

var paths = {
    'psc': ['src/**/*.purs', 'src/**/*.js'],
    'static': 'static/**/*',
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
    return gulp.src('static/scss/main.scss')
		.pipe(sass())
		.on('error', function(err) {
			console.log(err.message);
			this.emit('end');
		})
		.pipe(autoprefixer({
			browsers: 'last 2 versions'
		}))
		.pipe(minifyCss())
		.pipe(gulp.dest('dist/css/'));
});

gulp.task('copy', function(cb){
    runSequence('copy-index-html', 'copy-css', cb);
});

gulp.task('make', function () {
    return purescript.psc({ src: sources, ffi: foreigns });
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
    gulp.watch(paths.psc, function(){
        runSequence('make', 'bundle');
    });
    gulp.watch(paths.static, ['copy', 'bundle']);
});

gulp.task('default', function(cb){
    runSequence('clean', ['copy', 'make'], 'bundle', 'watch', cb);
});
