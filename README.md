# dnsplay
Straight forward and fast bulk DNS resolver
## Usage
From `-h`/`--help`:
```
  -r FILE  --resolvers=FILE  path to a file containing a list of DNS resolvers
  -t type  --type=type       type of record to look up, default 'A'
  -p N     --parallel=N      number of parallel threads
  -h       --help            display this help message
```

## Recipes
- Brute force subdomains:
```bash
$ dnsplay -r nameservers -t A -p 200 <<<"$(xargs < ~/repos/SecLists/Discovery/DNS/combined_subdomains.txt -I SUB echo SUB.tesla.com)"
```
- Reverse lookup
```bash
 $ dnsplay -r nameservers -t A -p 200 <<<"$(python <(echo 'from ipaddress import IPv4Network;\nfor i in IPv4Network("23.218.0.0/16"): print(".".join(reversed(str(i).split(".")))+".in-addr.arpa")'))"
```