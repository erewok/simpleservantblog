var gulp = require('gulp');
var concat = require('gulp-concat');
var minify = require('gulp-minify');
var cleanCSS = require('gulp-clean-css');

gulp.task('minify-css', function() {
  return gulp.src('../assets/css/*.css')
    .pipe(cleanCSS({compatibility: 'ie8'}))
    .pipe(concat('styles.min.css'))
    .pipe(gulp.dest('../assets/css'));
});

gulp.task('compress-js', function() {
  gulp.src('../assets/js/elm.js')
    .pipe(minify({
        ext:{
            src:'.js',
            min:'.min.js'
        }
    }))
    .pipe(gulp.dest('../assets/js'))
});

gulp.task('compress-admin-js', function() {
  gulp.src('../assets/js/admin-elm.js')
    .pipe(minify({
        ext:{
            src:'.js',
            min:'.min.js'
        }
    }))
    .pipe(gulp.dest('../assets/js'))
});

gulp.task('watch', function() {
  gulp.watch('../assets/**/*js', ['compress-js', 'compress-admin-js']);
  gulp.watch('../assets/**/*css', ['minify-css']);
});

gulp.task('default', ['minify-css', 'compress-js', 'compress-admin-js']);
