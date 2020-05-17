module.exports = {
  plugins: [
    require('tailwindcss'),
    require("autoprefixer"),
    require('postcss-purgecss')({
      content: [
          './src/**/*.elm',
          './index.html',
          './static/404.html'
      ],
      defaultExtractor: content => content.match(/[^<>"'`\s]*[^<>"'`\s:]/g) || []
    })
  ]
};