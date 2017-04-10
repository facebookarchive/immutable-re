const ChildProcess = require('child_process');
const Fs = require('fs');

const exec = cmd => new Promise(
  (resolve, reject) => {
    ChildProcess.exec(cmd, (err, stdout, stderr) => {
      if (err != null) {
        reject(err);
      } else {
        resolve({ stdout, stderr });
      }
    });
  }
);

const readFile = (fileName, encodingType) => new Promise (
  (resolve, reject) => {
    Fs.readFile(fileName, encodingType, (err, data) => {
      if (err != null) { reject(err); } else { resolve(data); }
    });
  }
);

const writeFile = (fileName, data) => new Promise (
  (resolve, reject) => {
    Fs.writeFile(fileName, data, err => {
      if (err != null) { reject(err); } else { resolve(); }
    });
  }
);

const opamFile = "opam";
const metaFile = "pkg/META";
const packageJSONFile = "package.json";

const writeFiles = async version => {
  const opam = `
opam-version: "1.2"
name: "immutable"
version: "${ version }"
maintainer: "Dave Bordoley <bordoley>"
authors: [ "Dave Bordoley <bordoley>" ]
license: "BSD"
homepage: "https://github.com/facebookincubator/immutable-re.git"
dev-repo: "git://github.com/facebookincubator/immutable-re.git"
bug-reports: "https://github.com/facebookincubator/immutable-re/issues"
tags: [ "reason" "immutable" ]
build: [
  [make "build"]
]
depends: [
  "topkg"       {build >=  "0.8.1" & < "0.9"}
  "reason"      {build >=  "1.13.3"}
]
available: [ ocaml-version >= "4.02" & ocaml-version < "4.05" ]
`;

  const meta =  `
# Copyright (c) 2017-present, Facebook, Inc. All rights reserved.

version = "${ version }"
description = "immutable: Immutable data structures for Reason"

archive(byte) = "immutable.cma"
archive(native) = "immutable.cmxa"
`;

  await writeFile (opamFile, opam);
  await writeFile (metaFile, meta);
};

const main = async () => {
  const packageJSON = await readFile (packageJSONFile, "UTF-8");
  const version = JSON.parse(packageJSON).version;

  await writeFiles (version);

  await exec(`git add ${ opamFile } ${ metaFile } ${ packageJSONFile }`)
  await exec(`git commit -m "Version ${ version }"`);
  await exec(`git tag -a ${ version } -m "Version ${ version }."`);
  await exec('git push "git@github.com:facebookincubator/immutable-re.git"');
  await exec(`git push "git@github.com:facebookincubator/immutable-re.git" tag ${version}`);
  await exec(`opam-publish prepare https://github.com/facebookincubator/immutable-re/archive/${ version }.tar.gz`);
  await exec(`opam-publish submit immutable.${ version }`);
  await exec('npm publish');
};


main ();
