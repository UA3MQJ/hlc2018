#include <QCoreApplication>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QFile>
#include <QTextStream>
#include <QDataStream>
#include <QDebug>
#include <QTime>
#include <QTimer>
#include <QTextCodec>
#include <QByteArray>

QByteArray unicode_to_win1251(QString value) {
    QTextCodec *codec = QTextCodec::codecForName("Windows-1251");
    QByteArray text = codec->fromUnicode(value);
    return text;
}

void write_str(QDataStream *stream, QByteArray str) {
    const char data = str.size();
    stream->writeRawData(&data, 1);
    stream->writeRawData(str.data(), str.size());
}

int main(int argc, char *argv[])
{
//    Q_UNUSED(argc);
//    Q_UNUSED(argv);
    QCoreApplication a(argc, argv);
    QStringList args = a.arguments();

    QString fname1 = args.at(1);
    QString fname2 = args.at(2);
    // qDebug() << fname1 << "\r";
    // qDebug() << fname2 << "\r";

    // QTime myTimer1, myTimer2;
    // myTimer1.start();

    QFile file(fname1);
    QFile file_out(fname2);

    if(!file.open(QIODevice::ReadOnly)) {
        qDebug() << "error opening file: " << file.error() << "\r";
        return -1;
    }

    if(!file_out.open(QIODevice::WriteOnly)) {
        qDebug() << "error opening file_out: " << file_out.error() << "\r";
        return -1;
    }

    QTextStream instream(&file);
    QString line = instream.readLine();

    // int nMilliseconds = myTimer1.elapsed();
    // qDebug() << "read json " << nMilliseconds << "ms" << "\r";

    QDataStream out_stream(&file_out);



    // myTimer2.start();
    QJsonDocument document = QJsonDocument::fromJson(line.toUtf8());
    QJsonObject object = document.object();
    QJsonValue value = object.value("accounts");
    QJsonArray array = value.toArray();

//    QTextCodec *codec = QTextCodec::codecForName("Windows-1251");
//    QByteArray text;

    foreach (const QJsonValue & v, array) {
        quint32 id = v.toObject().value("id").toInt();
        QByteArray email = unicode_to_win1251(v.toObject().value("email").toString());
        QByteArray sname = unicode_to_win1251(v.toObject().value("sname").toString());
        QByteArray fname = unicode_to_win1251(v.toObject().value("fname").toString());
        QByteArray phone = unicode_to_win1251(v.toObject().value("phone").toString());
        quint8 sex = (v.toObject().value("sex").toString()=="m") ? 0 : 255;
        quint32 birth = v.toObject().value("birth").toInt();
        QByteArray country = unicode_to_win1251(v.toObject().value("country").toString());
        QByteArray city = unicode_to_win1251(v.toObject().value("city").toString());
        quint32 joined = v.toObject().value("joined").toInt();

        quint8 status = 0;
        if (v.toObject().value("status").toString()=="свободны")   status = 1;
        if (v.toObject().value("status").toString()=="заняты")     status = 2;
        if (v.toObject().value("status").toString()=="всё сложно") status = 3;

        out_stream << id;
        write_str(&out_stream, email);
        write_str(&out_stream, sname);
        write_str(&out_stream, fname);
        write_str(&out_stream, phone);
        out_stream << sex;
        out_stream << birth;
        write_str(&out_stream, country);
        write_str(&out_stream, city);
        out_stream << joined;
        out_stream << status;

        QJsonValue interests_value = v.toObject().value("interests");
        QJsonArray interests_array = interests_value.toArray();
        quint8 interests_size = interests_array.size();
        out_stream << interests_size;
        if(interests_size > 0) {
            foreach (const QJsonValue & i_v, interests_array) {
                QByteArray interest = unicode_to_win1251(i_v.toString());
                write_str(&out_stream, interest);
            }
        }

        quint32 premium_start = 0;
        quint32 premium_finish = 0;
        QJsonValue premium = v.toObject().value("premium");
        if(!premium.toObject().value("start").isUndefined()) {
            premium_start = premium.toObject().value("start").toInt();
            premium_finish = premium.toObject().value("finish").toInt();
        }
        out_stream << premium_start;
        out_stream << premium_finish;

        QJsonValue likes_value = v.toObject().value("likes");
        QJsonArray likes_array = likes_value.toArray();
        quint8 likes_size = likes_array.size();
        out_stream << likes_size;
        if(likes_size > 0) {
            foreach (const QJsonValue & l_v, likes_array) {
                quint32 like_id = l_v.toObject().value("id").toInt();
                quint32 like_ts = l_v.toObject().value("ts").toInt();
                out_stream << like_id;
                out_stream << like_ts;
            }
        }


    }

    // nMilliseconds = myTimer2.elapsed();
    // qDebug() << fname1 << " " << nMilliseconds << " ms" << "\r";


    file.close();
    file_out.close();

   // QCoreApplication::exit();
   // return a.exec();
    return 0;
}
