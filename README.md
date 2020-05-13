# UniFi Markdown Extractor

Extracts configuration information about UniFi sites and converts it into Markdown for documentation purposes.

The tool generates a directory structure containing a Markdown files documenting the network and firewall configuration. 

## Disclaimer

This tool comes with no warranty of any kind. It may completely destroy your network or otherwise impair its functionality. Use at your own risk.

Do not use the tool on UniFi network equipment you are not authorised to access.

## Running

For general use the tool is intended to run inside a Docker container. This means users don't need to have anything but a Docker environment available.

### Credentials

Although you can use your UniFi controller administrative credentials it is recommended that you create a separate account with read only to all sites but no permissions to make modifications such as device adoption.

### Docker Compose

### sbt

The tool is currently built using Java 11 and has not been tested with other versions. Assuming you have a Scala environment with a configured JDK available you can set the environment variables and use `sbt run` to run the tool.

### Environment Variables

The tool requires four environment variables to be configured. 

* **SERVER_URL** The base URL of the UniFi controller
* **USERNAME** Username for authentication to the controller
* **PASSWORD** Password for authentication to the controller
* **BASE_PATH** Path into which the Markdown should be written

