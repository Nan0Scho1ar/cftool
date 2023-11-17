


### Definition of Terms

- `Template`: A file containing CloudFormation resource definitions used to create a `Stack`. This exists locally.
- `Configuration`: A set of properties defined in the config.yaml for deploying a `Stack` using a specific `Template`. This exist locally.
- `Stack`: A instance which has been provisioned inside CloudFormation. This exists in AWS.
- `Deployment`: A related set the above three pieces. A deployment consists of a `Stack` and also the `Template` and `Configuration` which was used to create it. This does really not "exist", in the sense that it's merely an abstract name for referring to the sum of it's pieces.

### Commands

``` sh
## List all available deployments found in the config.yml
cft list 

## Give an overview of the specified deployment
cft describe <configuration>:<instance>

## Diff the local Template and Configuration against the provisioned Stack in CloudFormation
cft diff <configuration>:<instance>





## Create a new Stack in CloudFormation using the local Template and Configuration
cft create <configuration>:<instance>

## Update an existing Stack in CloudFormation using the local Template and Configuration
cft update <configuration>:<instance>


## Delete the existing Stack in CloudFormation which is associated with this deployment
cft delete <configuration>:<instance>




## Compare the local templates of two distinct deployments
cft compare template <configuration>:<instance> <configuration>:<instance>

## Compare the local configurations of two distinct deployments
cft compare config   <configuration>:<instance> <configuration>:<instance>

## Compare the remote stacks of two distinct deployments
cft compare stack    <configuration>:<instance> <configuration>:<instance>
```
