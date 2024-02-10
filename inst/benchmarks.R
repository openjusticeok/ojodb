library(googleCloudStorageR)

bucket <- gcs_get_bucket("oscn")

gcs_list_objects("bucket")
