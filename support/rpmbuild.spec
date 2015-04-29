%define git_url git@git.ishopex.cn:base/bnow.git
%define rev %(date +%%Y%%m%%d%%H%%M)
%define __prelink_undo_cmd %{nil}

Name:           betternow
Version:        %{rev}
Release:        shopex
Summary:        betternow

Group:          Development/Tools
License:        ERPL
URL:            http://www.shopex.cn
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Source1:        betternow-init.script

BuildRequires:	m4
BuildRequires:	erlang >= R15
BuildRequires:  subversion, zeromq
Requires: php-common , php-fpm , zeromq

%description
ecae

%package sdk
Group:          Development/Tools
Summary: SDK for betternow

%description sdk
SDK for betternow

%prep
export LC_ALL=en_US.UTF-8
rm -rf bnow
git clone %{git_url} bnow
cd bnow
git checkout %{rev}

%build
cd bnow
./configure --prefix=/usr/local \
         --exec-prefix=%{_prefix} \
         --bindir=%{_bindir} \
         --libdir=%{_libdir}

make deps version all
GIT_SSL_NO_VERIFY=true DESTDIR=$RPM_BUILD_ROOT make release

%install
rm -rf $RPM_BUILD_ROOT
cd bnow
make DESTDIR=$RPM_BUILD_ROOT install
mkdir -p $RPM_BUILD_ROOT/etc/init.d
mkdir -p $RPM_BUILD_ROOT/usr/bin
make DESTDIR=$RPM_BUILD_ROOT -C apps/sdk install
cp %{SOURCE1} $RPM_BUILD_ROOT/etc/init.d/betternow

%clean
rm -rf $RPM_BUILD_ROOT
rm -rf bnow

%files
%defattr(-,root,root)
/usr/local/bnow
/etc/init.d/betternow
/usr/bin
%config(noreplace) /usr/local/bnow/etc/app.config
%config(noreplace) /usr/local/bnow/etc/vm.args
%exclude /usr/local/bnow/apps

%post
/sbin/ldconfig

%files sdk
/usr/local/bnow/apps/sdk

%changelog
