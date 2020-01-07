module.exports = {
  pluginOptions: {
    i18n: {
      locale: "en",
      fallbackLocale: "en",
      localeDir: "locales",
      enableInSFC: false,
    },
  },
  productionSourceMap: false,
  devServer: {
    port: 8085,
    serveIndex: true,
    before: (app, server, compiler) => {
      app.get(`${process.env.VUE_APP_DATA_URL}*`, (req, res) => {
        const fs = require("fs");
        const fileName = `${process.env.VUE_APP_EXAMPLE_PROJECT}${req.url.slice(process.env.VUE_APP_DATA_URL.length)}`;
        const file = fs.readFileSync(decodeURIComponent(fileName));
        res.end(file);
      });
    }
  }
};
