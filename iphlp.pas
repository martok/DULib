//From: http://www.mytips123.com/articles/53/1/How-to-get-the-local-DNS-servers-list%3F
//
//How to get the local DNS servers list?
//By hongbin fei | Published  06/9/2006 | Internet/LAN | Rating:

unit iphlp;

interface

uses Windows, Classes;

const
  MAX_HOSTNAME_LEN    = 128;
  MAX_DOMAIN_NAME_LEN = 128;
  MAX_SCOPE_ID_LEN    = 256;

type
  //
  // TIPAddressString - store an IP address or mask as dotted decimal string
  //
  PIPAddressString = ^TIPAddressString;
  PIPMaskString    = ^TIPAddressString;
  TIPAddressString = record
    _String: array[0..(4 * 4) - 1] of Char;
  end;
  TIPMaskString = TIPAddressString;

  //
  // TIPAddrString - store an IP address with its corresponding subnet mask,
  // both as dotted decimal strings
  //
  PIPAddrString = ^TIPAddrString;
  TIPAddrString = packed record
    Next: PIPAddrString;
    IpAddress: TIPAddressString;
    IpMask: TIPMaskString;
    Context: DWORD;
  end;

  //
  // FIXED_INFO - the set of IP-related information which does not depend on DHCP
  //
  PFixedInfo = ^TFixedInfo;
  TFixedInfo = packed record
    HostName: array[0..MAX_HOSTNAME_LEN + 4 - 1] of Char;
    DomainName: array[0..MAX_DOMAIN_NAME_LEN + 4 - 1] of Char;
    CurrentDnsServer: PIPAddrString;
    DnsServerList: TIPAddrString;
    NodeType: UINT;
    ScopeId: array[0..MAX_SCOPE_ID_LEN + 4 - 1] of Char;
    EnableRouting,
    EnableProxy,
    EnableDns: UINT;
  end;


function GetNetworkParams(pFixedInfo: PFixedInfo; pOutBufLen: PULONG): DWORD; stdcall;

  // Get machine DNS Servers and return them in the provided StringList. This list should have been
  // already created by the calling program before performing this call
procedure GetDNSServers(AList: TStringList);


implementation

const
  {$IFDEF MSWINDOWS}
  iphlpapidll = 'iphlpapi.dll';
  {$ENDIF}

function GetNetworkParams; external iphlpapidll Name 'GetNetworkParams';


procedure GetDNSServers(AList: TStringList);
var
  pFI: PFixedInfo;
  pIPAddr: PIPAddrString;
  OutLen: Cardinal;
begin
  AList.Clear;
  OutLen := SizeOf(TFixedInfo);
  GetMem(pFI, SizeOf(TFixedInfo));
  try
    if GetNetworkParams(pFI, @OutLen) = ERROR_BUFFER_OVERFLOW then
    begin
      ReallocMem(pFI, OutLen);
      if GetNetworkParams(pFI, @OutLen) <> NO_ERROR then Exit;
    end;
    // If there is no network available there may be no DNS servers defined
    if pFI^.DnsServerList.IpAddress._String[0] = #0 then Exit;
    // Add first server
    AList.Add(pFI^.DnsServerList.IpAddress._String);
    // Add rest of servers
    pIPAddr := pFI^.DnsServerList.Next;
    while Assigned(pIPAddr) do
    begin
      AList.Add(pIPAddr^.IpAddress._String);
      pIPAddr := pIPAddr^.Next;
    end;
  finally
    FreeMem(pFI);
  end;
end;

end.

