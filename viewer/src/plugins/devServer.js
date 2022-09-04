/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
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

/*
-- Create a file .env.development.local in the project to customize your DevServer
-- VUE_APP_DEVSERVER_CONFIG_PATH=<http_url> will use the dev_proxyconfig
-- VUE_APP_DEVSERVER_CONFIG_PATH=<fs_path> will use the dev_fsconfig
*/

const devReady = Boolean(process.env.VUE_APP_DEVSERVER_CONFIG_PATH);
const devIsProxy = devReady && Boolean(process.env.VUE_APP_DEVSERVER_CONFIG_PATH?.match(/^https?:\/\//i));
const devLocalpath = `^/${process.env.VUE_APP_DATA_URL}`;
const devProxyconfig = {
  [devLocalpath]: {
    target: process.env.VUE_APP_DEVSERVER_CONFIG_PATH,
    pathRewrite: { [devLocalpath]: '' },
  },
};

function devFsConfig(middlewares, devServer) {
  devServer.app.get(`${devLocalpath}*`, (req, res) => {
    const fs = require('fs');
    const url = req.url.slice(process.env.VUE_APP_DATA_URL?.length);
    const paramIdx = url.indexOf('?');
    const filepath = paramIdx < 0 ? url : url.substring(0, paramIdx);
    const fullpath = `${process.env.VUE_APP_DEVSERVER_CONFIG_PATH}${decodeURIComponent(filepath)}`;
    const file = fs.readFileSync(fullpath);
    res.end(file);
  });
  return middlewares;
}

module.exports = {
  port: process.env.VUE_APP_DEVSERVER_PORT,
  proxy: devReady && devIsProxy ? devProxyconfig : undefined,
  setupMiddlewares: devReady && !devIsProxy ? devFsConfig : undefined,
};
