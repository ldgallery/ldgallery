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

const { defineConfig } = require('@vue/cli-service');
const devServer = require('./src/plugins/devServer');

module.exports = defineConfig({
  publicPath: './',
  transpileDependencies: false,
  productionSourceMap: false,

  devServer,
  pluginOptions: {
  },

  configureWebpack: {
    cache: {
      type: 'filesystem',
    },
    watchOptions: {
      ignored: /node_modules/,
    },
  },

  chainWebpack: config => {
    // Integrates the YAML loader, for i18n files
    config.module
      .rule('yaml')
      .test(/\.ya?ml$/)
      .use('yaml')
      .loader('js-yaml-loader');
    config.plugin('define')
      .tap(args => {
        args[0].__VUE_I18N_FULL_INSTALL__ = false;
        args[0].__VUE_I18N_LEGACY_API__ = false;
        return args;
      });
  },
});
