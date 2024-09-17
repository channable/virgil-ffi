{ buildPythonPackage
, cbor2
, channalib
, fqdn
, google-cloud-storage
, heliclockter
, more-itertools
, opentelemetry-api
, opentelemetry-exporter-otlp
, opentelemetry-sdk
, psycopg2
, pydantic
, requests
, setuptools
, sqlalchemy
, urllib3
, tblib
}:
buildPythonPackage {
  pname = "imaginator-client";
  version = "0.1";
  format = "pyproject";
  src = channalib.fileFilter {
    name = "imaginator-client-src";
    src = ./.;
    srcWhitelist = [
      "pyproject.toml"
      "imaginator(/.*)?$"
    ];
    srcGlobalBlacklist = [
      "__pycache__"
    ];
    srcGlobalWhitelist = [
      ".py"
      ".toml"
      "py.typed"
    ];
  };

  buildInputs = [
    setuptools
  ];

  propagatedBuildInputs = [
    cbor2
    fqdn
    google-cloud-storage
    heliclockter
    opentelemetry-api
    opentelemetry-exporter-otlp
    opentelemetry-sdk
    more-itertools
    psycopg2
    pydantic
    requests
    sqlalchemy
    urllib3
  ];
}
