#! /bin/bash

mkdir ${{ github.workspace }}/certs
echo "$OJO_SSL_CERT_BASE64" | base64 --decode > ${{ github.workspace }}/certs/client-cert.pem
echo "$OJO_SSL_ROOT_CERT_BASE64" | base64 --decode > ${{ github.workspace }}/certs/server-ca.pem
echo "$OJO_SSL_KEY_BASE64" | base64 --decode > ${{ github.workspace }}/certs/client-key.pk8
chmod 0600 ${{ github.workspace }}/certs/client-key.pk8
