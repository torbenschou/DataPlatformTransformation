DECLARE @culture_info       NVARCHAR(25)
DECLARE @max_rows_to_return BIGINT 

SET @culture_info = 'en'
SET @max_rows_to_return = 1000



SELECT hicv.[ComputerName]                                                                                    AS 'Computer Name',
			CASE
               WHEN d.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type',
			'' AS "Operated By",
			'' AS Environment, 
			Model																									AS 'System Model',
			SiteName,
             s.[Name]                                                                                               AS 'SQL Server Instance Name',
             [Common].[GetSqlVersionDisplayString](sdv.[VersionCoalesce], @culture_info)                            AS 'SQL Server Product Name',
             sdv.[VersionCoalesce]                                                                                  AS 'SQL Server Version',
             [SqlServer_Reporting].[ConvertToSpLevel](sdv.[Splevel])                                                AS 'SQL Server Service Pack',
             COALESCE([SqlServer_Reporting].[_GetSqlEditionDisplayString](sdv.[Skuname], @culture_info), 'Unknown') AS 'SQL Server Edition',
            [Collation]																								AS 'Collation',
			 case WHEN [SqlServer_Reporting].[ConvertToYesOrNo](sdv.[Clustered], @culture_info) = 'No'
					THEN 0
					ELSE 1
						END                              AS 'Clustered?',
             sdv.[Vsname]                                                                                           AS 'SQL Server Cluster Network Name',
             s.[State]                                                                                              AS 'SQL Service State',
             s.[StartMode]                                                                                          AS 'SQL Service Start Mode',
			 (SELECT count(x.[DeviceNumber])
				FROM [SqlServer_Reporting].[SqlDbinstanceDatabasesView] x
			    WHERE  x.[DeviceNumber] = d.[DeviceNumber])															AS 'Number Of Databases',
             hicv.[CurrentOperatingSystem]                                                                          AS 'Current Operating System',
             CASE
               WHEN d.[OsArchitecture] IS NULL THEN
                 CASE
                   WHEN d.[OsCaption] LIKE '%64%' THEN '64-bit'
                   ELSE '32-bit'
                 END
               ELSE d.[OsArchitecture]
             END                                                                                                    AS 'Operating System Architecture Type',
             [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber])                                 AS 'Number of Processors',
             CASE 
				WHEN [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber])  IS NULL 
				THEN  [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber]) 
				ELSE  [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber]) END                                  AS 'Number of Total Cores',
             [AllDevices_Assessment].[_GetLogicalProcessorCount](d.[DeviceNumber])                                  AS 'Number of Logical Processors',
             (SELECT DISTINCT( ( ( LTRIM(p.[Name]) ) + ', ' + LTRIM(p.[DataWidth]) ) + ' bit' + CHAR(10) ) AS [text()]
              FROM   [Win_Inventory].[Processors] AS p
              WHERE  p.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'CPU',
             CAST (CAST(d.[TotalPhysicalMemory] AS BIGINT) / 1024 / 1024 AS NVARCHAR(256))                          AS 'System Memory (MB)',
             (SELECT LTRIM(COALESCE(ld.[Caption], 'N/A')) + CHAR(10) AS [text()]
              FROM   [Win_Inventory].[LogicalDisks] AS ld
              WHERE  ld.[DeviceNumber] = d.[DeviceNumber]
              ORDER  BY ld.[Caption]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Drive Name',
			  (SELECT sum(ld1.[Size]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld1
              WHERE  ld1.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Size (GB)',
			 (SELECT sum(ld2.[FreeSpace]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld2
              WHERE  ld2.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))																					AS 'Logical Disk Free Space (GB)',
			floor(NULLIF(stat.[SuccessfullAttempts],0)/Cast(stat.[TotalAttempts] as decimal(18,0))  * 100) SuccessPercent,
			ROUND(paa.[CpuPercentagePercentile], 2)                                    AS [ProcUtil],
			(paa.[TotalBytes] - paa.[AvailableBytesPercentile]) / (1024 * 1024 * 1024) AS [MemoryUtil],
			ROUND(paa.[NetBytesPerSecPercentile] / (1024 * 1024), 2)                   AS [NetworkUtil],
			ROUND(paa.[DiskIopsPercentile], 2)                                         AS [DiskIOPS] 
		FROM   [Core_Inventory].[Devices] d
             INNER JOIN [SqlServer_Reporting].[SqlDbInstancesView] sdv
               ON d.[DeviceNumber] = sdv.[DeviceNumber]
             INNER JOIN [AllDevices_Assessment].[HardwareInventoryCoreView] hicv
               ON d.[DeviceNumber] = hicv.[DeviceNumber]
             LEFT OUTER JOIN [Win_Inventory].[Services] s
               ON sdv.[DeviceNumber] = s.[DeviceNumber]
                  AND RTRIM(sdv.[Servicename]) = RTRIM(s.[Name])
             LEFT OUTER JOIN (SELECT hgd1.[DeviceNumber],
                                     hgd1.[GuestDeviceNumber]
                              FROM   [HyperV_Inventory].[HostGuestDetails] hgd1
                                     INNER JOIN [Core_Assessment].[UniqueDevices] ud
                                       ON hgd1.[DeviceNumber] = ud.[DeviceNumber]) hgd
               ON d.[DeviceNumber] = hgd.[GuestDeviceNumber]
			JOIN [Perf_Assessment].[Statistics] stat
			 ON d.[DeviceNumber] = stat.[DeviceNumber]
    LEFT OUTER JOIN [Perf_Assessment].[PerformanceAggregationAssessmentView] paa
      ON paa.[DeviceNumber] = stat.[DeviceNumber] 


--
-- SQL INSTANCE REPORT
--

SELECT hicv.[ComputerName] AS 'Computer Name'
              ,CASE
               WHEN d.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type'
			 ,'' AS "Operated By"
			 ,'' AS Environment                                                                                    
             ,s.[Name]                                                                                               AS 'SQL Server Instance Name',
             [Common].[GetSqlVersionDisplayString](sdv.[VersionCoalesce], @culture_info)                            AS 'SQL Server Product Name',
             sdv.[VersionCoalesce]                                                                                  AS 'SQL Server Version',
             [SqlServer_Reporting].[ConvertToSpLevel](sdv.[Splevel])                                                AS 'SQL Server Service Pack',
             COALESCE([SqlServer_Reporting].[_GetSqlEditionDisplayString](sdv.[Skuname], @culture_info), 'Unknown') AS 'SQL Server Edition',
             [SqlServer_Reporting].[ConvertToYesOrNo](sdv.[Clustered], @culture_info)                               AS 'Clustered?',
             sdv.[Vsname]                                                                                           AS 'SQL Server Cluster Network Name',
             s.[State]                                                                                              AS 'SQL Service State',
             s.[StartMode]                                                                                          AS 'SQL Service Start Mode',
             (SELECT count(x.[DeviceNumber])
				FROM [SqlServer_Reporting].[SqlDbinstanceDatabasesView] x
			    WHERE  x.[DeviceNumber] = d.[DeviceNumber])															AS 'Number Of Databases',
             hicv.[CurrentOperatingSystem]                                                                          AS 'Current Operating System',
             hicv.[OsServicePackForDisplay]                                                                         AS 'Operating System Service Pack Level',
             CASE
               WHEN d.[OsArchitecture] IS NULL THEN
                 CASE
                   WHEN d.[OsCaption] LIKE '%64%' THEN '64-bit'
                   ELSE '32-bit'
                 END
               ELSE d.[OsArchitecture]
             END                                                                                                    AS 'Operating System Architecture Type'
      FROM   [Core_Inventory].[Devices] d
             INNER JOIN [SqlServer_Reporting].[SqlDbInstancesView] sdv
               ON d.[DeviceNumber] = sdv.[DeviceNumber]
             INNER JOIN [AllDevices_Assessment].[HardwareInventoryCoreView] hicv
               ON d.[DeviceNumber] = hicv.[DeviceNumber]
             LEFT OUTER JOIN [Win_Inventory].[Services] s
               ON sdv.[DeviceNumber] = s.[DeviceNumber]
                  AND RTRIM(sdv.[Servicename]) = RTRIM(s.[Name])
             LEFT OUTER JOIN (SELECT hgd1.[DeviceNumber],
                                     hgd1.[GuestDeviceNumber]
                              FROM   [HyperV_Inventory].[HostGuestDetails] hgd1
                                     INNER JOIN [Core_Assessment].[UniqueDevices] ud
                                       ON hgd1.[DeviceNumber] = ud.[DeviceNumber]) hgd
               ON d.[DeviceNumber] = hgd.[GuestDeviceNumber]
 				INNER JOIN [Perf_Assessment].[Statistics] paa
				ON  d.[DeviceNumber] = paa.[DeviceNumber] 
			        ORDER  BY 1


------
------ SQL COMPONENTS
------


    SELECT hic.[ComputerName]																		 AS 'Computer Name',
                           CASE
               WHEN d.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type'
			 ,'' AS "Operated By"
			 ,'' AS Environment   
			 ,CASE
               WHEN sscbd.[Sqlservicetype] = 4 THEN 'SQL Server Integration Services'
               WHEN sscbd.[Sqlservicetype] = 5 THEN 'SQL Server Analysis Services'
               WHEN sscbd.[Sqlservicetype] = 6 THEN 'SQL Server Reporting Services'
               WHEN sscbd.[Sqlservicetype] = 101 THEN 'SQL Master Data Services'
               WHEN sscbd.[Sqlservicetype] = 102 THEN 'SQL Data Quality Services'
			   WHEN sscbd.[Sqlservicetype] = 12 THEN 'SQL Server R Services'
             END                                                                                     AS 'SQL Server Component Name',
             sscbd.[Version]                                                                         AS 'Component Version',
             sscbd.[Splevel]                                                                         AS 'Component Service Pack',
             sscbd.[Skuname]                                                                         AS 'SQL Server Component Edition',
             hicv.[OperatingSystemForDisplay]                                                        AS 'Current Operating System',
             hicv.[OsServicePackForDisplay]                                                          AS 'Operating System Service Pack Level',
             COALESCE(d.[OsArchitecture], CASE
                                            WHEN d.[OsCaption] LIKE '%64%' THEN '64-bit'
                                            ELSE '32-bit'
                                          END)                                                       AS 'Operating System Architecture Type'
             --[AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber])                  AS 'Number of Processors',
             --[AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber])                       AS 'Number of Total Cores',
             --[AllDevices_Assessment].[_GetLogicalProcessorCount](d.[DeviceNumber])                   AS 'Number of Logical Processors',
             --(SELECT DISTINCT( ( ( LTRIM(p.[Name]) ) + ', ' + LTRIM(p.[DataWidth]) ) + ' bit' + CHAR(10) ) AS [text()]
             -- FROM   [Win_Inventory].[Processors] AS p
             -- WHERE  p.[DeviceNumber] = d.[DeviceNumber]
             -- FOR XML PATH (''))                                                                     AS 'CPU',
             --CAST (CAST(d.[TotalPhysicalMemory] AS BIGINT) / 1024 / 1024 AS NVARCHAR(256))           AS 'System Memory (MB)',
             --(SELECT LTRIM(COALESCE(ld.[Caption], 'N/A')) + CHAR(10) AS [text()]
             -- FROM   [Win_Inventory].[LogicalDisks] AS ld
             -- WHERE  ld.[DeviceNumber] = d.[DeviceNumber]
             -- ORDER  BY ld.[Caption]
             -- FOR XML PATH (''))                                                                     AS 'Logical Disk Drive Name',
             --(SELECT LTRIM(COALESCE(CAST (CAST(ld1.[Size] AS BIGINT) / 1024 / 1024 / 1024 AS NVARCHAR(256)), 'N/A')) + CHAR(10) AS [text()]
             -- FROM   [Win_Inventory].[LogicalDisks] AS ld1
             -- WHERE  ld1.[DeviceNumber] = d.[DeviceNumber]
             -- ORDER  BY ld1.[Caption]
             -- FOR XML PATH (''))                                                                     AS 'Logical Disk Size (GB)',
             --(SELECT LTRIM(COALESCE(CAST (CAST(ld2.[FreeSpace] AS BIGINT) / 1024 / 1024 / 1024 AS NVARCHAR(256)), 'N/A')) + CHAR(10) AS [text()]
             -- FROM   [Win_Inventory].[LogicalDisks] AS ld2
             -- WHERE  ld2.[DeviceNumber] = d.[DeviceNumber]
             -- ORDER  BY ld2.[Caption]
             -- FOR XML PATH (''))                                                                     AS 'Logical Disk Free Space (GB)',
             --[AllDevices_Assessment].[_GetWmiScanResultForDisplay](d.[WmiScanResult], @culture_info) AS 'WMI Status'
            FROM   [Core_Inventory].[Devices] d
             INNER JOIN [AllDevices_Assessment].[HardwareInventoryCore] hic
               ON d.[DeviceNumber] = hic.[DeviceNumber]
             INNER JOIN [SqlServer_Reporting].[_GetSqlServersComponentsByDevice](@culture_info) sscbd
               ON d.[DeviceNumber] = sscbd.[DeviceNumber] AND sscbd.[Sqlservicetype] != 1
             INNER JOIN [AllDevices_Assessment].[HardwareInventoryCoreView] hicv 
               ON d.[DeviceNumber] = hicv.[DeviceNumber]
 				INNER JOIN [Perf_Assessment].[Statistics] paa
				ON  d.[DeviceNumber] = paa.[DeviceNumber] 
      ORDER  BY 1
 


------
------ Windows servers hosting SQL Server components
------


SELECT DISTINCT [ComputerName]
,CASE
               WHEN b.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type'
			 ,'' AS "Operated By"
			 ,'' AS Environment   
			 ,[OsFamilyName]
      ,[CurrentOperatingSystem]
      ,[OsServicePack]
      ,[OsArchitectureForDisplay]
	  ,c.AdOsVersion
	  ,c.AdFullyQualifiedDomainName
	  ,c.LastBootupTime
	  ,c.Model
	  ,c.SiteName
	  ,count(b.DeviceId) as NumberOfVolumes
	  ,c.Roles
FROM [AllDevices_Assessment].[HardwareInventoryCoreView] a
  JOIN [Win_Inventory].[LogicalDisks] b
  ON a.DeviceNumber = b.DeviceNumber
    JOIN [Core_Inventory].[Devices] c
	ON a.DeviceNumber = c.DeviceNumber
	 INNER JOIN [SqlServer_Reporting].[SqlInstancesCategorizationView] hac
 ON a.DeviceNumber = hac.DeviceNumber
  	INNER JOIN [Perf_Assessment].[Statistics] paa
	ON  a.[DeviceNumber] = paa.[DeviceNumber] 	
  GROUP BY 
		[ComputerName]
      ,[OsFamilyName]
      ,[CurrentOperatingSystem]
      ,[OsServicePack]
      ,[OsArchitectureForDisplay]
	  ,c.AdFullyQualifiedDomainName
	  ,c.AdOsVersion
	  ,c.LastBootupTime
	  ,c.Model
	  ,c.Roles
	  ,c.SiteName
	  ,b.[DeviceNumber]
	        ORDER  BY 1


----
---- Hardware Information of servers hosting SQL Server components
----

			
select distinct 
hi.[ComputerName]				AS 'Computer Name',
CASE
               WHEN hi.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type'
			 ,'' AS "Operated By"
			 ,'' AS Environment        
,hi.[ComputerModel]				AS 'Computer Model',
hi.[NumberOfProcessors]			AS 'Number of Processors',
hi.[NumberOfCores]				AS 'Number of Cores',
hi.[NumberOfLogicalProcessors]  AS 'Logical Processor Count',
hi.[Cpu]						AS 'CPU Model',
hi.[SystemMemory]				AS 'System Memory (MB)',
hi.[SystemMemory]/1024			AS 'System Memory (GB)',
             (SELECT LTRIM(COALESCE(ld.[Caption], 'N/A')) + CHAR(10) AS [text()]
              FROM   [Win_Inventory].[LogicalDisks] AS ld
              WHERE  ld.[DeviceNumber] = hi.[DeviceNumber]
              ORDER  BY ld.[Caption]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Drive Name',
             (SELECT LTRIM(COALESCE(CAST (CAST(ld1.[Size] AS BIGINT) / 1024 / 1024 / 1024 AS NVARCHAR(256)), 'N/A')) + CHAR(10) AS [text()]
              FROM   [Win_Inventory].[LogicalDisks] AS ld1
              WHERE  ld1.[DeviceNumber] = hi.[DeviceNumber]
              ORDER  BY ld1.[Caption]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Size (GB)',
             (SELECT LTRIM(COALESCE(CAST (CAST(ld2.[FreeSpace] AS BIGINT) / 1024 / 1024 / 1024 AS NVARCHAR(256)), 'N/A')) + CHAR(10) AS [text()]
              FROM   [Win_Inventory].[LogicalDisks] AS ld2
              WHERE  ld2.[DeviceNumber] = hi.[DeviceNumber]
              ORDER  BY ld2.[Caption]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Free Space (GB)',
			  (SELECT sum(ld1.[Size]) / 1024 / 1024 / 1024 
			  FROM   [Win_Inventory].[LogicalDisks] AS ld1
              WHERE  ld1.[DeviceNumber] = hi.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'Total Logical Disk Size (GB)',
			  (SELECT sum(ld2.[FreeSpace]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld2
              WHERE  ld2.[DeviceNumber] = hi.[DeviceNumber]
              FOR XML PATH (''))																					AS 'Total Logical Disk Free Space (GB)',   
hi.[DiskDrive]				AS 'Disk Drive',
--hi.[MachineType]			AS 'Machine Type',
hi.[ActiveNetworkAdapter]	AS 'Active Network Adapter',
hi.[IPAddress]				AS 'IP Address',
hi.[MACAddress]				AS 'MAC Address',
hi.[DNSServer]				AS 'DNS Server',
hi.[SubnetMask]				AS 'Subnet Mask',
hi.[WINSServer]				AS 'WINS Server'
 FROM [AllDevices_Assessment].[HardwareInventoryView] hi
 INNER JOIN [SqlServer_Reporting].[SqlInstancesCategorizationView] hac
 ON hi.DeviceNumber = hac.DeviceNumber
	INNER JOIN [Perf_Assessment].[Statistics] paa
	ON  hi.[DeviceNumber] = paa.[DeviceNumber] 	
ORDER BY hi.ComputerName 

--
-- Performance of servers hosting SQL Server components
--

  SELECT DISTINCT
    hic.[ComputerName]                                                         AS [ComputerName],
	CASE
               WHEN hi.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type'
			 ,'' AS "Operated By"
			 ,'' AS Environment 
	,floor(NULLIF(stat.[SuccessfullAttempts],0)/Cast(stat.[TotalAttempts] as decimal(18,0))  * 100) SuccessPercent, 
    ROUND(paa.[CpuPercentagePercentile], 2)                                    AS [ProcUtil],
    (paa.[TotalBytes] - paa.[AvailableBytesPercentile]) / (1024 * 1024 * 1024) AS [MemoryUtil],
    ROUND(paa.[NetBytesPerSecPercentile] / (1024 * 1024), 2)                   AS [NetworkUtil],
    ROUND(paa.[DiskIopsPercentile], 2)                                         AS [DiskIOPS]
  FROM
    [Perf_Assessment].[Statistics] stat
    INNER JOIN [AllDevices_Assessment].[HardwareInventoryCore] hic
      ON stat.[DeviceNumber] = hic.[DeviceNumber]
    LEFT OUTER JOIN [Perf_Assessment].[PerformanceAggregationAssessmentView] paa
      ON paa.[DeviceNumber] = stat.[DeviceNumber]
	   JOIN [AllDevices_Assessment].[HardwareInventoryView] hi
	on stat.[DeviceNumber] = hi.[DeviceNumber]
	 INNER JOIN [SqlServer_Reporting].[SqlInstancesCategorizationView] hac
 ON hi.DeviceNumber = hac.DeviceNumber
	--where SuccessfullAttempts > 1
ORDER BY ComputerName 


--
-- Performance detail of servers hosting SQL Server components
--
SELECT DISTINCT hi.[ComputerName]						  AS 'Machine Name'
				,hi.[MachineType]							  AS 'Machine Type'
			 ,'' AS "Operated By"
			 ,'' AS Environment 
             ,hi.[CurrentOperatingSystem]                  AS 'Operating System',
			 hi.[Cpu]                                     AS 'CPU',
             Cast(wi.MaxClockSpeed AS FLOAT) / 1000		  AS 'CPU Speed (GHz)',
			hi.[NumberOfProcessors]			AS 'Number of Processors',
			hi.[NumberOfCores]				AS 'Number of Cores',
			hi.[NumberOfLogicalProcessors]  AS 'Logical Processor Count',			 
             [AverageCpuUtilization]                      AS 'Average CPU Utilization (%)',
             [MaximumCpuUtilization]                      AS 'Maximum CPU Utilization (%)',
             [CpuUtilization95thPercentile]               AS '95th Percentile CPU Utilization (%)',
             hi.[SystemMemory]                            AS 'Memory (MB)',
             AverageMemoryUtilizationInGB                 AS 'Average Memory Utilization (GB)',
             MaximumMemoryUtilizationInGB                 AS 'Maximum Memory Utilization (GB)',
             MemoryUtilizationInGB95thPercentile          AS '95th Percentile Memory Utilization (GB)',
             AverageDiskIOPS                              AS 'Average Disk IOPS',
             MaximumDiskIOPS                              AS 'Maximum Disk IOPS',
             DiskIOPS95thPercentile                       AS '95th Percentile Disk IOPS',
             AverageDiskWritesPerSec                      AS 'Avg Disk Writes/sec',
             MaximumDiskWritesPerSec                      AS 'Max Disk Writes/sec',
             DiskWritesPerSec95thPercentile               AS '95th Percentile Disk Writes/sec',
             AverageDiskReadsPerSec                       AS 'Avg Disk Reads/sec',
             MaximumDiskReadsPerSec                       AS 'Max Disk Reads/sec',
             DiskReadsPerSec95thPercentile                AS '95th Percentile Disk Reads/sec',
             AverageNetworkUtilizationInMBps              AS 'Average Network Utilization (MB/s)',
             MaximumNetworkUtilizationInMBps              AS 'Maximum Network Utilization (MB/s)',
             NetworkUtilizationInMBps95thPercentile       AS '95th Percentile Network Utilization (MB/s)',
             AverageNetworkByesSendPerSecInMBps           AS 'Avg Network Bytes Sent (MB/s)',
             MaximumNetworkByesSendPerSecInMBps           AS 'Max Network Bytes Sent (MB/s)',
             NetworkBytesSendPerSecInMBpsPercentile       AS '95th Percentile Network Bytes Sent (MB/s)',
             AverageNetworkBytesRecvPerSecInMBps          AS 'Avg Network Bytes Received (MB/s)',
             MaximumNetworkBytesRecvPerSecInMBps          AS 'Max Network Bytes Received (MB/s)',
             NetworkBytesRecvPerSecInMBpsPercentile       AS '95th Percentile Network Bytes Received (MB/s)',
             --hi.[DiskDrive]                               AS 'Disk Drive',
             --hi.[DiskDriveSize]                           AS 'Disk Drive Size (GB)',
             AverageDiskSpaceUtilizationInGB              AS 'Average Disk Space Utilization (GB)',
             MaximumDiskSpaceUtilizationInGB              AS 'Maximum Disk Space Utilization (GB)',
             DiskSpaceUtilizationInGB95thPercentile       AS '95th Percentile Disk Space Utilization (GB)',			 
			 AverageDiskQueueLength						  AS 'Avg Disk Queue Length',
			 MaximumDiskQueueLength						  AS 'Max Disk Queue Length',
			 DiskQueueLengthPercentile					  AS '95th Percentile Disk Queue Length',
			 AverageDiskReadQueueLength					  AS 'Avg Disk Read Queue Length',
			 MaximumDiskReadQueueLength					  AS 'Max Disk Read Queue Length',
			 DiskReadQueueLengthPercentile				  AS '95th Percentile Disk Read Queue Length',
			 AverageDiskWriteQueueLength				  AS 'Avg Disk Write Queue Length',
			 MaximumDiskWriteQueueLength				  AS 'Max Disk Write Queue Length',
			 DiskWriteQueueLengthPercentile				  AS '95th Percentile Disk Write Queue Length',
			 AverageDiskBytesSec						  AS 'Avg Disk Bytes/sec',
			 MaximumDiskBytesSec						  AS 'Max Disk Bytes/sec',
			 DiskBytesSecPercentile						  AS '95th Percentile Disk Bytes/sec'			 
      FROM   [Perf_Reporting].[GetPlacementMetricsSummary] () gpms      
      LEFT OUTER JOIN [AllDevices_Assessment].[HardwareInventoryView] hi
      ON hi.[DeviceNumber] = gpms.[DeviceNumber]
	  LEFT OUTER JOIN [Win_Inventory].[Processors] wi
      ON wi.[DeviceNumber] = gpms.[DeviceNumber]
	  	 INNER JOIN [SqlServer_Reporting].[SqlInstancesCategorizationView] hac
 ON hi.DeviceNumber = hac.DeviceNumber
  	  INNER JOIN [Perf_Assessment].[Statistics] paa
      ON  hi.[DeviceNumber] = paa.[DeviceNumber]
ORDER BY ComputerName 



--Consolidation overview of servers hosting SQL Server components



SELECT hicv.[ComputerName]                                                                                    AS 'Computer Name',
			CASE
               WHEN d.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type',
			'' AS "Operated By",
			'' AS Environment, 
			Model																									AS 'System Model',
			SiteName,
             s.[Name]                                                                                               AS 'SQL Server Instance Name',
             [Common].[GetSqlVersionDisplayString](sdv.[VersionCoalesce], @culture_info)                            AS 'SQL Server Product Name',
             sdv.[VersionCoalesce]                                                                                  AS 'SQL Server Version',
             [SqlServer_Reporting].[ConvertToSpLevel](sdv.[Splevel])                                                AS 'SQL Server Service Pack',
             COALESCE([SqlServer_Reporting].[_GetSqlEditionDisplayString](sdv.[Skuname], @culture_info), 'Unknown') AS 'SQL Server Edition',
            [Collation]																								AS 'Collation',
			 [SqlServer_Reporting].[ConvertToYesOrNo](sdv.[Clustered], @culture_info)                               AS 'Clustered?',
             sdv.[Vsname]                                                                                           AS 'SQL Server Cluster Network Name',
             s.[State]                                                                                              AS 'SQL Service State',
             s.[StartMode]                                                                                          AS 'SQL Service Start Mode',
			 (SELECT count(x.[DeviceNumber])
				FROM [SqlServer_Reporting].[SqlDbinstanceDatabasesView] x
			    WHERE  x.[DeviceNumber] = d.[DeviceNumber])															AS 'Number Of Databases',
             hicv.[CurrentOperatingSystem]                                                                          AS 'Current Operating System',
             CASE
               WHEN d.[OsArchitecture] IS NULL THEN
                 CASE
                   WHEN d.[OsCaption] LIKE '%64%' THEN '64-bit'
                   ELSE '32-bit'
                 END
               ELSE d.[OsArchitecture]
             END                                                                                                    AS 'Operating System Architecture Type',
             [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber])                                 AS 'Number of Processors',
             CASE 
				WHEN [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber])  IS NULL 
				THEN  [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber]) 
				ELSE  [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber]) END                                  AS 'Number of Total Cores',
             [AllDevices_Assessment].[_GetLogicalProcessorCount](d.[DeviceNumber])                                  AS 'Number of Logical Processors',
             (SELECT DISTINCT( ( ( LTRIM(p.[Name]) ) + ', ' + LTRIM(p.[DataWidth]) ) + ' bit' + CHAR(10) ) AS [text()]
              FROM   [Win_Inventory].[Processors] AS p
              WHERE  p.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'CPU',
             CAST (CAST(d.[TotalPhysicalMemory] AS BIGINT) / 1024 / 1024 AS NVARCHAR(256))                          AS 'System Memory (MB)',
             (SELECT LTRIM(COALESCE(ld.[Caption], 'N/A')) + CHAR(10) AS [text()]
              FROM   [Win_Inventory].[LogicalDisks] AS ld
              WHERE  ld.[DeviceNumber] = d.[DeviceNumber]
              ORDER  BY ld.[Caption]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Drive Name',
			  (SELECT sum(ld1.[Size]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld1
              WHERE  ld1.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Size (GB)',
			 (SELECT sum(ld2.[FreeSpace]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld2
              WHERE  ld2.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))																					AS 'Logical Disk Free Space (GB)',
			floor(NULLIF(stat.[SuccessfullAttempts],0)/Cast(stat.[TotalAttempts] as decimal(18,0))  * 100) SuccessPercent, 
			ROUND(paa.[CpuPercentagePercentile], 2)                                    AS [ProcUtil],
			(paa.[TotalBytes] - paa.[AvailableBytesPercentile]) / (1024 * 1024 * 1024) AS [MemoryUtil],
			ROUND(paa.[NetBytesPerSecPercentile] / (1024 * 1024), 2)                   AS [NetworkUtil],
			ROUND(paa.[DiskIopsPercentile], 2)                                         AS [DiskIOPS] 
		FROM   [Core_Inventory].[Devices] d
             INNER JOIN [SqlServer_Reporting].[SqlDbInstancesView] sdv
               ON d.[DeviceNumber] = sdv.[DeviceNumber]
             INNER JOIN [AllDevices_Assessment].[HardwareInventoryCoreView] hicv
               ON d.[DeviceNumber] = hicv.[DeviceNumber]
             LEFT OUTER JOIN [Win_Inventory].[Services] s
               ON sdv.[DeviceNumber] = s.[DeviceNumber]
                  AND RTRIM(sdv.[Servicename]) = RTRIM(s.[Name])
             LEFT OUTER JOIN (SELECT hgd1.[DeviceNumber],
                                     hgd1.[GuestDeviceNumber]
                              FROM   [HyperV_Inventory].[HostGuestDetails] hgd1
                                     INNER JOIN [Core_Assessment].[UniqueDevices] ud
                                       ON hgd1.[DeviceNumber] = ud.[DeviceNumber]) hgd
               ON d.[DeviceNumber] = hgd.[GuestDeviceNumber]
			JOIN [Perf_Assessment].[Statistics] stat
			 ON d.[DeviceNumber] = stat.[DeviceNumber]
    LEFT OUTER JOIN [Perf_Assessment].[PerformanceAggregationAssessmentView] paa
      ON paa.[DeviceNumber] = stat.[DeviceNumber] 
      ORDER  BY 1

--
-- Consolidation target overview - servers with running instances 
--


SELECT distinct [Computer Name]
      ,[Machine Type]
      ,[Operated By]
      ,[Environment]
      ,[System Model]
      ,[SiteName]
      --,[SQL Server Instance Name]
      --,[SQL Server Product Name]
      --,[SQL Server Version]
      --,[SQL Server Service Pack]
      --,[SQL Server Edition]
      --,[Collation]
      ,sum([Clustered?]) [NumberOfClusteredInstances] 
      --,[SQL Server Cluster Network Name]
      --,[SQL Service State]
      --,[SQL Service Start Mode]
      --,[Number Of Databases]
      ,[Current Operating System]
      ,[Operating System Architecture Type]
	  ,count([SQL Server Instance Name]) as 'Number of SQL Instances'
	  ,sum([SQL Services Running]) as 'SQL Instances running'
	  ,sum([SQL Services Stopped]) as 'SQL Instances stopped'  
	  ,sum([Number Of Databases]) as [Number Of Databases]
      ,[Number of Processors]
      ,[Number of Total Cores]
      ,[Number of Logical Processors]
      ,[CPU]
      ,[System Memory (MB)]
	  ,cast(round(cast([System Memory (MB)] as decimal (18,2))/1024,0) as INT) as [System Memory (GB)]
      --,[Logical Disk Drive Name]
      ,[Logical Disk Size (GB)]
      ,[Logical Disk Free Space (GB)]
	  ,[Logical Disk Size (GB)]/1024 [Logical Disk Size (TB)]
      ,[Logical Disk Free Space (GB)]/1024 [Logical Disk Free Space (TB)]
      ,[SuccessPercent]
	  ,'' Description
      ,[ProcUtil]
      ,[MemoryUtil]
      ,[NetworkUtil]
      ,[DiskIOPS]
  FROM (SELECT hicv.[ComputerName]                                                                                    AS 'Computer Name',
			CASE
               WHEN d.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type',
			'' AS "Operated By",
			'' AS Environment, 
			Model																									AS 'System Model',
			SiteName,
             s.[Name]                                                                                               AS 'SQL Server Instance Name',
             [Common].[GetSqlVersionDisplayString](sdv.[VersionCoalesce], @culture_info)                            AS 'SQL Server Product Name',
             sdv.[VersionCoalesce]                                                                                  AS 'SQL Server Version',
             [SqlServer_Reporting].[ConvertToSpLevel](sdv.[Splevel])                                                AS 'SQL Server Service Pack',
             COALESCE([SqlServer_Reporting].[_GetSqlEditionDisplayString](sdv.[Skuname], @culture_info), 'Unknown') AS 'SQL Server Edition',
            [Collation]																								AS 'Collation',
			 case WHEN [SqlServer_Reporting].[ConvertToYesOrNo](sdv.[Clustered], @culture_info) = 'No'
					THEN 0
					ELSE 1
						END                              AS 'Clustered?',
             sdv.[Vsname]                                                                                           AS 'SQL Server Cluster Network Name',
             s.[State]                                                                                              AS 'SQL Service State',
			 CASE
               WHEN s.[State] = 'Running' THEN 1
			   ELSE 0
			   END                                                                                             AS 'SQL Services Running',
              CASE
               WHEN s.[State] = 'Stopped' THEN 1
			   ELSE 0
			   END                                                                                             AS 'SQL Services Stopped',
              s.[StartMode]                                                                                          AS 'SQL Service Start Mode',
			 (SELECT count(x.[DeviceNumber])
				FROM [SqlServer_Reporting].[SqlDbinstanceDatabasesView] x
			    WHERE  x.[DeviceNumber] = d.[DeviceNumber])															AS 'Number Of Databases',
             hicv.[CurrentOperatingSystem]                                                                          AS 'Current Operating System',
             CASE
               WHEN d.[OsArchitecture] IS NULL THEN
                 CASE
                   WHEN d.[OsCaption] LIKE '%64%' THEN '64-bit'
                   ELSE '32-bit'
                 END
               ELSE d.[OsArchitecture]
             END                                                                                                    AS 'Operating System Architecture Type',
             [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber])                                 AS 'Number of Processors',
             CASE 
				WHEN [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber])  IS NULL 
				THEN  [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber]) 
				ELSE  [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber]) END                                  AS 'Number of Total Cores',
             [AllDevices_Assessment].[_GetLogicalProcessorCount](d.[DeviceNumber])                                  AS 'Number of Logical Processors',
             (SELECT DISTINCT( ( ( LTRIM(p.[Name]) ) + ', ' + LTRIM(p.[DataWidth]) ) + ' bit' + CHAR(10) ) AS [text()]
              FROM   [Win_Inventory].[Processors] AS p
              WHERE  p.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'CPU',
             CAST (CAST(d.[TotalPhysicalMemory] AS BIGINT) / 1024 / 1024 AS NVARCHAR(256))                          AS 'System Memory (MB)',
             (SELECT LTRIM(COALESCE(ld.[Caption], 'N/A')) + CHAR(10) AS [text()]
              FROM   [Win_Inventory].[LogicalDisks] AS ld
              WHERE  ld.[DeviceNumber] = d.[DeviceNumber]
              ORDER  BY ld.[Caption]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Drive Name',
			  (SELECT sum(ld1.[Size]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld1
              WHERE  ld1.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Size (GB)',
			 (SELECT sum(ld2.[FreeSpace]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld2
              WHERE  ld2.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))																					AS 'Logical Disk Free Space (GB)',
			floor(NULLIF(stat.[SuccessfullAttempts],0)/Cast(stat.[TotalAttempts] as decimal(18,0))  * 100) SuccessPercent,
			ROUND(paa.[CpuPercentagePercentile], 2)                                    AS [ProcUtil],
			(paa.[TotalBytes] - paa.[AvailableBytesPercentile]) / (1024 * 1024 * 1024) AS [MemoryUtil],
			ROUND(paa.[NetBytesPerSecPercentile] / (1024 * 1024), 2)                   AS [NetworkUtil],
			ROUND(paa.[DiskIopsPercentile], 2)                                         AS [DiskIOPS] 
		FROM   [Core_Inventory].[Devices] d
             INNER JOIN [SqlServer_Reporting].[SqlDbInstancesView] sdv
               ON d.[DeviceNumber] = sdv.[DeviceNumber]
             INNER JOIN [AllDevices_Assessment].[HardwareInventoryCoreView] hicv
               ON d.[DeviceNumber] = hicv.[DeviceNumber]
             LEFT OUTER JOIN [Win_Inventory].[Services] s
               ON sdv.[DeviceNumber] = s.[DeviceNumber]
                  AND RTRIM(sdv.[Servicename]) = RTRIM(s.[Name])
             LEFT OUTER JOIN (SELECT hgd1.[DeviceNumber],
                                     hgd1.[GuestDeviceNumber]
                              FROM   [HyperV_Inventory].[HostGuestDetails] hgd1
                                     INNER JOIN [Core_Assessment].[UniqueDevices] ud
                                       ON hgd1.[DeviceNumber] = ud.[DeviceNumber]) hgd
               ON d.[DeviceNumber] = hgd.[GuestDeviceNumber]
			JOIN [Perf_Assessment].[Statistics] stat
			 ON d.[DeviceNumber] = stat.[DeviceNumber]
    LEFT OUTER JOIN [Perf_Assessment].[PerformanceAggregationAssessmentView] paa
      ON paa.[DeviceNumber] = stat.[DeviceNumber]  
) x
  group by 
  [Computer Name]
      ,[Machine Type]
      ,[Operated By]
      ,[Environment]
      ,[System Model]
      ,[SiteName]
      ,[Current Operating System]
      ,[Operating System Architecture Type]
      ,[Number of Processors]
      ,[Number of Total Cores]
      ,[Number of Logical Processors]
      ,[CPU]
      ,[System Memory (MB)]
      ,[Logical Disk Drive Name]
      ,[Logical Disk Size (GB)]
      ,[Logical Disk Free Space (GB)]
      ,[SuccessPercent]
      ,[ProcUtil]
      ,[MemoryUtil]
      ,[NetworkUtil]
      ,[DiskIOPS]
  HAVING sum([SQL Services Running]) > 0
  order by 1

--
-- Consolidation target overview - servers with only stop instances (cluster resources)
--


SELECT distinct [Computer Name]
      ,[Machine Type]
      ,[Operated By]
      ,[Environment]
      ,[System Model]
      ,[SiteName]
      --,[SQL Server Instance Name]
      --,[SQL Server Product Name]
      --,[SQL Server Version]
      --,[SQL Server Service Pack]
      --,[SQL Server Edition]
      --,[Collation]
      ,sum([Clustered?]) [NumberOfClusteredInstances] 
      --,[SQL Server Cluster Network Name]
      --,[SQL Service State]
      --,[SQL Service Start Mode]
      --,[Number Of Databases]
      ,[Current Operating System]
      ,[Operating System Architecture Type]
	  ,count([SQL Server Instance Name]) as 'Number of SQL Instances'
	  ,sum([SQL Services Running]) as 'SQL Instances running'
	  ,sum([SQL Services Stopped]) as 'SQL Instances stopped'  
	  ,sum([Number Of Databases]) as [Number Of Databases]
      ,[Number of Processors]
      ,[Number of Total Cores]
      ,[Number of Logical Processors]
      ,[CPU]
      ,[System Memory (MB)]
	  ,cast(round(cast([System Memory (MB)] as decimal (18,2))/1024,0) as INT) as [System Memory (GB)]
      --,[Logical Disk Drive Name]
      ,[Logical Disk Size (GB)]
      ,[Logical Disk Free Space (GB)]
	  ,[Logical Disk Size (GB)]/1024 [Logical Disk Size (TB)]
      ,[Logical Disk Free Space (GB)]/1024 [Logical Disk Free Space (TB)]
      ,[SuccessPercent]
	  ,'' Description
      ,[ProcUtil]
      ,[MemoryUtil]
      ,[NetworkUtil]
      ,[DiskIOPS]
  FROM (SELECT hicv.[ComputerName]                                                                                    AS 'Computer Name',
			CASE
               WHEN d.[DeviceNumber] IN (SELECT cd.[DeviceNumber]
                                         FROM   [AllDevices_Assessment].[CategorizedDevices] cd
                                         WHERE  [IsVirtual] = 1) THEN 'Virtual'
               ELSE 'Physical'
             END                                                                                                    AS 'Machine Type',
			'' AS "Operated By",
			'' AS Environment, 
			Model																									AS 'System Model',
			SiteName,
             s.[Name]                                                                                               AS 'SQL Server Instance Name',
             [Common].[GetSqlVersionDisplayString](sdv.[VersionCoalesce], @culture_info)                            AS 'SQL Server Product Name',
             sdv.[VersionCoalesce]                                                                                  AS 'SQL Server Version',
             [SqlServer_Reporting].[ConvertToSpLevel](sdv.[Splevel])                                                AS 'SQL Server Service Pack',
             COALESCE([SqlServer_Reporting].[_GetSqlEditionDisplayString](sdv.[Skuname], @culture_info), 'Unknown') AS 'SQL Server Edition',
            [Collation]																								AS 'Collation',
			 case WHEN [SqlServer_Reporting].[ConvertToYesOrNo](sdv.[Clustered], @culture_info) = 'No'
					THEN 0
					ELSE 1
						END                              AS 'Clustered?',
             sdv.[Vsname]                                                                                           AS 'SQL Server Cluster Network Name',
             s.[State]                                                                                              AS 'SQL Service State',
			 CASE
               WHEN s.[State] = 'Running' THEN 1
			   ELSE 0
			   END                                                                                             AS 'SQL Services Running',
              CASE
               WHEN s.[State] = 'Stopped' THEN 1
			   ELSE 0
			   END                                                                                             AS 'SQL Services Stopped',
              s.[StartMode]                                                                                          AS 'SQL Service Start Mode',
			 (SELECT count(x.[DeviceNumber])
				FROM [SqlServer_Reporting].[SqlDbinstanceDatabasesView] x
			    WHERE  x.[DeviceNumber] = d.[DeviceNumber])															AS 'Number Of Databases',
             hicv.[CurrentOperatingSystem]                                                                          AS 'Current Operating System',
             CASE
               WHEN d.[OsArchitecture] IS NULL THEN
                 CASE
                   WHEN d.[OsCaption] LIKE '%64%' THEN '64-bit'
                   ELSE '32-bit'
                 END
               ELSE d.[OsArchitecture]
             END                                                                                                    AS 'Operating System Architecture Type',
             [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber])                                 AS 'Number of Processors',
             CASE 
				WHEN [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber])  IS NULL 
				THEN  [AllDevices_Assessment].[_GetPhysicalProcessorCount](d.[DeviceNumber]) 
				ELSE  [AllDevices_Assessment].[_GetPhysicalCoreCount](d.[DeviceNumber]) END                                  AS 'Number of Total Cores',
             [AllDevices_Assessment].[_GetLogicalProcessorCount](d.[DeviceNumber])                                  AS 'Number of Logical Processors',
             (SELECT DISTINCT( ( ( LTRIM(p.[Name]) ) + ', ' + LTRIM(p.[DataWidth]) ) + ' bit' + CHAR(10) ) AS [text()]
              FROM   [Win_Inventory].[Processors] AS p
              WHERE  p.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'CPU',
             CAST (CAST(d.[TotalPhysicalMemory] AS BIGINT) / 1024 / 1024 AS NVARCHAR(256))                          AS 'System Memory (MB)',
             (SELECT LTRIM(COALESCE(ld.[Caption], 'N/A')) + CHAR(10) AS [text()]
              FROM   [Win_Inventory].[LogicalDisks] AS ld
              WHERE  ld.[DeviceNumber] = d.[DeviceNumber]
              ORDER  BY ld.[Caption]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Drive Name',
			  (SELECT sum(ld1.[Size]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld1
              WHERE  ld1.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))                                                                                    AS 'Logical Disk Size (GB)',
			 (SELECT sum(ld2.[FreeSpace]) / 1024 / 1024 / 1024 
              FROM   [Win_Inventory].[LogicalDisks] AS ld2
              WHERE  ld2.[DeviceNumber] = d.[DeviceNumber]
              FOR XML PATH (''))																					AS 'Logical Disk Free Space (GB)',
			floor(NULLIF(stat.[SuccessfullAttempts],0)/Cast(stat.[TotalAttempts] as decimal(18,0))  * 100) SuccessPercent,
			ROUND(paa.[CpuPercentagePercentile], 2)                                    AS [ProcUtil],
			(paa.[TotalBytes] - paa.[AvailableBytesPercentile]) / (1024 * 1024 * 1024) AS [MemoryUtil],
			ROUND(paa.[NetBytesPerSecPercentile] / (1024 * 1024), 2)                   AS [NetworkUtil],
			ROUND(paa.[DiskIopsPercentile], 2)                                         AS [DiskIOPS] 
		FROM   [Core_Inventory].[Devices] d
             INNER JOIN [SqlServer_Reporting].[SqlDbInstancesView] sdv
               ON d.[DeviceNumber] = sdv.[DeviceNumber]
             INNER JOIN [AllDevices_Assessment].[HardwareInventoryCoreView] hicv
               ON d.[DeviceNumber] = hicv.[DeviceNumber]
             LEFT OUTER JOIN [Win_Inventory].[Services] s
               ON sdv.[DeviceNumber] = s.[DeviceNumber]
                  AND RTRIM(sdv.[Servicename]) = RTRIM(s.[Name])
             LEFT OUTER JOIN (SELECT hgd1.[DeviceNumber],
                                     hgd1.[GuestDeviceNumber]
                              FROM   [HyperV_Inventory].[HostGuestDetails] hgd1
                                     INNER JOIN [Core_Assessment].[UniqueDevices] ud
                                       ON hgd1.[DeviceNumber] = ud.[DeviceNumber]) hgd
               ON d.[DeviceNumber] = hgd.[GuestDeviceNumber]
			JOIN [Perf_Assessment].[Statistics] stat
			 ON d.[DeviceNumber] = stat.[DeviceNumber]
    LEFT OUTER JOIN [Perf_Assessment].[PerformanceAggregationAssessmentView] paa
      ON paa.[DeviceNumber] = stat.[DeviceNumber]  
) x
  group by 
  [Computer Name]
      ,[Machine Type]
      ,[Operated By]
      ,[Environment]
      ,[System Model]
      ,[SiteName]
      ,[Current Operating System]
      ,[Operating System Architecture Type]
      ,[Number of Processors]
      ,[Number of Total Cores]
      ,[Number of Logical Processors]
      ,[CPU]
      ,[System Memory (MB)]
      ,[Logical Disk Drive Name]
      ,[Logical Disk Size (GB)]
      ,[Logical Disk Free Space (GB)]
      ,[SuccessPercent]
      ,[ProcUtil]
      ,[MemoryUtil]
      ,[NetworkUtil]
      ,[DiskIOPS]
  HAVING sum([SQL Services Running]) = 0
  order by 1
