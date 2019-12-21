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
      app.get("/gallery/*", (req, res) => {
        const fs = require("fs");
        const fileName = req.url.replace(/^\/gallery/, "../example");
        const file = fs.readFileSync(decodeURIComponent(fileName));
        res.end(file);
      });
    }
  }
};
