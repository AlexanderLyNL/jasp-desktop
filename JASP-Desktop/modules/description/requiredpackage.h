#ifndef REQUIREDPACKAGE_H
#define REQUIREDPACKAGE_H

#include "descriptionchildbase.h"
#include "../upgrader/version.h"
#include "jsonredirect.h"

namespace Modules
{

struct PackageError  : public std::runtime_error
{
	PackageError(QString pkg, QString problem);
	const char* what() const noexcept override;
};

class RequiredPackage : public DescriptionChildBase
{
	Q_OBJECT

	Q_PROPERTY(QString name		READ name		WRITE setName		NOTIFY nameChanged		)
	Q_PROPERTY(QString version	READ versionStr	WRITE setVersionStr	NOTIFY versionChanged	)
	Q_PROPERTY(QString github	READ github		WRITE setGithub		NOTIFY githubChanged	)
	Q_PROPERTY(QString gitref	READ gitref		WRITE setGitref		NOTIFY gitrefChanged	)

public:
	RequiredPackage() : DescriptionChildBase() {}

	QString name()			const { return _name;						}
	Version version()		const { return _version;					}
	QString versionStr()	const { return tq(_version.toString(3));	}
	QString github()		const { return _github;						}
	QString gitref()		const { return _gitref;	}

	Json::Value	asJson() const;


public slots:
	void setName(		QString name);
	void setVersion(	Version version);
	void setVersionStr(	QString version) { setVersion(Version(fq(version))); }
	void setGithub(		QString github);
	void setGitref(		QString gitref);

signals:
	void nameChanged(	QString name);
	void versionChanged();
	void githubChanged(	QString github);
	void gitrefChanged(	QString gitref);

private:
	QString _name,
			_github,
			_gitref;
	Version _version;
};

}

#endif // REQUIREDPACKAGE_H
