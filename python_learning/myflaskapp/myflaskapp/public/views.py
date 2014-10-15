# -*- coding: utf-8 -*-
'''Public section, including homepage and signup.'''
from flask import (Blueprint, request, render_template, flash, url_for,
                    redirect, session)
from flask.ext.login import login_user, login_required, logout_user

from myflaskapp.extensions import login_manager
from myflaskapp.user.models import User
from myflaskapp.public.forms import LoginForm
from myflaskapp.public.forms import GetAllDataForm
from myflaskapp.user.forms import RegisterForm
from myflaskapp.utils import flash_errors
from myflaskapp.database import db

from flask import current_app
from flask_mail import Mail
from flask_mail import Message

blueprint = Blueprint('public', __name__, static_folder="../static")

@login_manager.user_loader
def load_user(id):
    return User.get_by_id(int(id))


@blueprint.route("/", methods=["GET", "POST"])
def home():
    form = LoginForm(request.form)
    # Handle logging in
    if request.method == 'POST':
        if form.validate_on_submit():
            login_user(form.user)
            flash("You are logged in.", 'success')
            redirect_url = request.args.get("next") or url_for("user.members")
            return redirect(redirect_url)
        else:
            flash_errors(form)
    return render_template("public/home.html", form=form)

@blueprint.route('/logout/')
@login_required
def logout():
    logout_user()
    flash('You are logged out.', 'info')
    return redirect(url_for('public.home'))

@blueprint.route("/register/", methods=['GET', 'POST'])
def register():
    form = RegisterForm(request.form, csrf_enabled=False)
    if form.validate_on_submit():
        new_user = User.create(username=form.username.data,
                        email=form.email.data,
                        password=form.password.data,
                        active=True)
        flash("Thank you for registering. You can now log in.", 'success')
        return redirect(url_for('public.home'))
    else:
        flash_errors(form)
    return render_template('public/register.html', form=form)

@blueprint.route("/about/")
def about():
    form = LoginForm(request.form)
    return render_template("public/about.html", form=form)

@blueprint.route('/alldata/', methods = ['GET', 'POST'])
@login_required
def alldata():
    form = GetAllDataForm(request.form, csrf_enabled = False)
    if form.validate_on_submit():
        sid  = form.sid.data
        date = form.date.data
        return redirect(url_for('public.data', query_sid = sid, query_date = date))
    else:
        flash_errors(form)
    return render_template('data/alldata.html', form = form)

@blueprint.route('/data/<query_sid>/<query_date>', methods = ['GET'])
@login_required
def data(query_sid, query_date):
    return render_template('data/returndata.html', query_sid = query_sid, query_date = query_date)


@blueprint.route('/mail/')
@login_required
def mail():
    print current_app.config

    msg = Message("hello", sender = "from@example.com", recipients = ['to@example.com'])
    msg.body = "testing"
    msg.html = "<b>testing</b>"

    mail = Mail()
    mail.init_app(current_app)

    mail.send(msg)

    return "ok"