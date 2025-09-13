/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import path = require('node:path');
import os = require('node:os');
import { ExtensionContext, window } from 'vscode';

import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export async function activate(_context: ExtensionContext) {
  const command = process.env.SERVER_PATH || 'ts_query_ls';
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
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'scheme' }],
  };

  client = new LanguageClient(
    'ts_query_ls',
    "LSP implementation for Tree-sitter's query files",
    serverOptions,
    clientOptions,
  );
  client.start();
}

export function deactivate(): Promise<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
