/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

module.exports = {
  publicPath: "./",
  pluginOptions: {
    i18n: {
      locale: "en",
      fallbackLocale: "en",
      localeDir: "locales",
      enableInSFC: false,
    },
  },
  chainWebpack: config => {
    config.plugins.delete("prefetch");
  },
  configureWebpack: {
    devtool: "source-map",
  },
  productionSourceMap: false,
  devServer: {
    port: process.env.VUE_APP_DEVSERVER_PORT,
    serveIndex: true,
    before: (app, server, compiler) => {
      app.get(`/${process.env.VUE_APP_DATA_URL}*`, (req, res) => {
        const fs = require("fs");
        const url = req.url.slice(process.env.VUE_APP_DATA_URL.length);
        const paramIdx = url.indexOf("?");
        const filepath = paramIdx < 0 ? url : url.substring(0, paramIdx);
        const fullpath = `${process.env.VUE_APP_DEVSERVER_CONFIG_PATH}${decodeURIComponent(filepath)}`;
        const file = fs.readFileSync(fullpath);
        res.end(file);
      });
    },
  },
};
