"""Contains MinIO utilities."""

# The size in bytes of the 'parts' during multipart uploads.
# Large files can't be uploaded in a single request. Multipart
# uploads allow uploading large files in smaller 'parts'. The
# parts can be uploaded in parallel and in any order.
# Upon completion, the server will stitch the parts back together
# into a single object.
# The default part size of the AWS CLI is currently 8 megabytes.
# So we will use the same part size here.
DEFAULT_MULTIPART_UPLOAD_PART_SIZE = 8 * 1024 * 1024  # 8 MiB

# The default endpoint for MinIO hosted at Deltares.
DEFAULT_MINIO_HOSTNAME = "s3.deltares.nl"
DEFAULT_MINIO_WEB_HOSTNAME = "s3-console.deltares.nl"
