import os = require('os');
import path = require('path');
import { ExtensionContext, workspace } from 'vscode';

import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(_: ExtensionContext) {
  // NOTE: Replace with path to ts_query_ls
  const command = 'ts_query_ls';
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_LOG: 'debug',
      },
    },
  };

  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'scheme' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
    },
    initializationOptions: {
      parser_install_directories: [
        path.join(
          os.homedir(),
          '/.local/share/nvim/lazy/nvim-treesitter/parser/',
        ),
      ],
    },
  };

  client = new LanguageClient(
    'ts_query_ls',
    'ts_query_ls',
    serverOptions,
    clientOptions,
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
