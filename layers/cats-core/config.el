;;; config.el --- cats-core

;;; Commentary:

;; My personal configuration.

;;; Code:

(spacemacs|defvar-company-backends shell-mode)

(defvar cats/projectile-require-project-root
  "Require projectile root.")

(defvar cats//projectile-curr nil
  "The current projectile project.")
(defvar cats/project-hook nil
  "Hooks run when a cats/project is fired.")

(defvar cats//executable-eslint nil)
(defvar cats/eslint-executable-hook nil
  "Hooks run when cats//executable-eslint is changed.")

(defvar cats//executable-jscs nil)

(defvar cats/jscs-executable-hook nil
  "Hooks run when cats//executable-jscs is changed.")

(defvar cats//executable-jshint nil)

(defvar cats/jshint-executable-hook nil
  "Hooks run when cats//executable-jshint is changed.")

(defvar cats//executable-tidy nil)

(defvar cats/tidy-executable-hook nil
  "Hooks run when cats//executable-tidy is changed.")

(defvar cats//executable-mocha nil)
(defvar cats/mocha-executable-hook nil
  "Hooks run when cats//executable-mocha is changed.")

(defvar cats//executable-babel-node nil)
(defvar cats/babel-node-executable-hook nil
  "Hooks run when cats//executable-babel-node is changed.")

(defvar cats//executable-coffeelint nil)
(defvar cats/coffeelint-executable-hook nil
  "Hooks run when cats//executable-coffeelint is changed.")

(defvar cats//executable-node nil)
(defvar cats/node-executable-hook nil
  "Hooks run when cats//executable-node is changed.")

(defvar cats//executable-phantomjs nil)
(defvar cats/phantomjs-executable-hook nil
  "Hooks run when cats//executable-phantomjs is changed.")

(defvar cats//executable-handlebars nil)
(defvar cats/handlebars-executable-hook nil
  "Hooks run when cats//executable-handlebars is changed.")

(defvar cats//executable-find nil)
(defvar cats/find-executable-hook nil
  "Hooks run when cats//executable-find is changed.")

(defvar cats/projectile-dir-root nil
  "The current dir root of the projectile project.")

(defvar cats/projectile-dir-base nil
  "The current base dir of the projectile project.")

(defvar cats//frame-width nil
  "Frame width.")

(defvar cats//frame-height nil
  "Frame height.")

(defvar buffer/force-save-some-buffers t
  "Force save buffers when focus is lost.")

(defvar buffer/do-not-kill-important-buffers t
  "Do not kill important buffer.")

;; Don't kill the important buffers
(defvar buffer/do-not-kill-buffer-names
  '("*scratch*"
    "*Messages*"
    "*Require Times*")
  "Names of buffers that should not be killed.")

(defvar desktop/desktop-dirname (concat spacemacs-cache-directory "desktop/")
  "Folder where to save desktop sessions.")

(defvar desktop/desktop-base-file-name
  (concat "emacs_" emacs-version-short ".desktop")
  "File names of desktop files.")

(defvar desktop/desktop-base-lock-name
  (concat "emacs_" emacs-version-short ".desktop.lock")
  "Desktop lock file name.")

(defvar cats/projectile-enable-caching nil
  "Enable projectile caching.")

(defvar cats/projectile-require-project-root nil
  "Require project root for projectile.")



;; [[http://www.emacswiki.org/emacs/EshellAlias][shell aliases]]

(defalias 'e 'find-file)
(defalias 'ff 'find-file)

;; What about =gd= to call the Diff command?
(defalias 'gd 'magit-diff-unstaged)
(defalias 'gds 'magit-diff-staged)


;; tramp
;; (defvar disable-tramp-backups '("ssh" "sftp" "su" "sudo"))
(defvar cats/disable-tramp-backups '(all))


;; aws

;; A collection of yasnippet snippets to be used with AWS CLI

(defcustom aws-snippets-regions '("us-east-1" "eu-west-1" "ap-southeast-1")
  "List of AWS regions for selections."
  :tag "regions"
  :type '(choice (string :tag "Region")
                 (repeat :tag "List of Regions"
                         (string :tag "Region")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-profiles '("test" "prod")
  "List of AWS profiles for selections."
  :tag "profiles"
  :type '(choice (string :tag "Profile")
                 (repeat :tag "List of Profiles"
                         (string :tag "Profiles")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-filters '("tag:Name" "tag:Project" "tag:Type" "tag:Cluster" "instance.group-id" "ip-address" "private-ip-address" "network-interface.subnet-id" "description" "name" "iam-instance-profile.arn")
  "List of AWS filters for selections."
  :tag "default-filters"
  :type '(choice (string :tag "Filter")
                 (repeat :tag "List of Filters"
                         (string :tag "Filter")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-ami-ls-query '"reverse(sort_by(Images, &CreationDate)[].{CreationDate:CreationDate,Name:Name,ImageId:ImageId,Public:Public,Description:Description})"
  "Query string used for ami-ls."
  :tag "ami-ls-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-autoscaling-describe-auto-scaling-groups-query '"AutoScalingGroups[].{Name: AutoScalingGroupName, Desired: DesiredCapacity,Min: MinSize, Max: MaxSize}"
  "Query string used for autoscaling-describe-auto-scaling-groups."
  :tag "autoscaling-describe-auto-scaling-groups"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-autoscaling-describe-launch-configurations-query '"reverse(sort_by(LaunchConfigurations &CreatedTime)[*].[LaunchConfigurationName, CreatedTime])"
  "Query string used for autoscaling-describe-launch-configurations."
  :tag "autoscaling-describe-launch-configurations-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-dns-names-query '"Reservations[].Instances[?State.Name=='running'].[InstanceId, PublicDnsName]"
  "Query string used for aws-ec2-get-dns-names."
  :tag "ec2-get-dns-names-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-instance-volumes-query '"Reservations[].Instances[].[join(\`,\`,Tags[?Key==\`Name\`].Value),join(' ', BlockDeviceMappings[].Ebs.VolumeId)]"
  "Query string used for ec2-get-instance-volumes."
  :tag "ec2-get-instance-volumes-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-securitygroups-query '"sort_by(SecurityGroups, &GroupName)[*].[GroupName, GroupId, Description]"
  "Query string used for ec2-get-securitygroups."
  :tag "ec2-get-securitygroups-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-snapshots-query '"Snapshots[].[join(\`,\`,Tags[?Key==\`Name\`].Value),StartTime,Description,SnapshotId,VolumeId,VolumeSize,Progress]"
  "Query string used for ec2-get-snapshots-query."
  :tag "ec2-get-snapshots-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-volumes-query '"Volumes[].[VolumeId,State,SnapshotId,join(\`,\`,Tags[?Key==\`Name\`].Value)]"
  "Query string used for ec2-get-volumes-query."
  :tag "ec2-get-volumes-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-volumes-unused-query '"Volumes[].[VolumeId,Size,VolumeType,CreateTime,State,SnapshotId]"
  "Query string used for ec2-get-volumes-unused-query."
  :tag "ec2-get-volumes-unused-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-instance-running-query '"Reservations[].Instances[].[InstanceId, State.Name]"
  "Query string used for ec2-instance-running."
  :tag "ec2-instance-running-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-list-instances-query '("Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime]"
						   "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]"
						   "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, KeyName]")
  "Query string used for ec2 describe-instances."
  :tag "ec2-list-instances-query"
  :type '(choice (string :tag "Query")
                 (repeat :tag "List of Queries"
                         (string :tag "Query")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-ec2-list-subnets-query '"sort_by(sort_by(Subnets[].{VpcId:VpcId, SubnetId:SubnetId, State:State, AvailabilityZone:AvailabilityZone, CidrBlock:CidrBlock, AvailableIpAddressCount:AvailableIpAddressCount Tags:to_string(Tags[].[Key, Value])}, &SubnetId), &VpcId)"
  "Query string used for ec2-list-subnets."
  :tag "ec2-list-subnets-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-list-vpcs-query '"sort_by(Vpcs[].{VpcId:VpcId, State:State, AvailabilityZone:AvailabilityZone, CidrBlock:CidrBlock, AvailableIpAddressCount:AvailableIpAddressCount Tags:to_string(Tags[].[Key, Value])}, &VpcId)"
  "Query string used for ec2-list-vpcs."
  :tag "ec2-list-vpcs-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-elb-get-list-query '"reverse(sort_by(LoadBalancerDescriptions, &CreatedTime)[].[LoadBalancerName, CreatedTime])"
  "Query string used for elb-get-list."
  :tag "elb-get-list-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-emr-list-clusters-query '"Clusters[*].{ID: Id, NAME: Name}"
  "Query string used for emr-list-clusters."
  :tag "emr-list-clusters-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-rds-describe-instances-query '"DBInstances[*].[DBInstanceIdentifier,DbiResourceId, Endpoint.Address]"
  "Query string used for rds describe-db-instances."
  :tag "rds-describe-db-instances-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-route53-list-hosted-zones-query '"HostedZones[].[Id, Name]"
  "Query string used for route53 list-hosted-zones."
  :tag "route53-list-hosted-zones-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-aws-support-ls-query '"reverse(sort_by(cases, &timeCreated)[].[caseId, subject, timeCreated, displayId])"
  "Query string used for aws support-ls-query."
  :tag "aws-support-ls-query"
  :group 'aws-snippets
  :type 'string)

;;; config.el ends here
