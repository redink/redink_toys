import os
import flaskr
import unittest
import tempfile

class FlaskrTestCase(unittest.TestCase):
    """docstring for FlaskrTestCase"unittest.TestCase
    def __init__(self, arg):
        super(FlaskrTestCase,unittest.TestCase).__init__()
        self.arg = arg
    """
    def setUp(self):
        self.db_fd, flaskr.app.config['DATABASE'] = tempfile.mkstemp()
        flaskr.app.config['TESTING'] = True
        self.app = flaskr.app.test_client()
        flaskr.init_db()

    def tearDown(self):
        os.close(self.db_fd)
        os.unlink(flaskr.app.config['DATABASE'])

    def test_empty_db(self):
        rv = self.app.get('/')
        assert 'No entries here so far' in rv.data

    def login(self, username, password):
        return self.app.post('/login', data = dict(
            username = username,
            password = password
        ), follow_redirects = True)

    def logout():
        return self.app.get('/logout', follow_redirects = True)


    def test_login_logout(self):
        rv = self.login('admin', 'default')
        assert 'you were logged in' in rv.data
        '''
        rv = self.logout()
        assert 'logged out' in rv.data
        '''
        rv = self.login('adminx', 'default')
        assert 'invalid username' in rv.data

        rv = self.login('admin', '123')
        assert 'invalid password' in rv.data

    def test_messages(self):
        self.login('admin', 'default')
        rv = self.app.post('/add', data=dict(
            title='<Hello>',
            text='<strong>HTML</strong> allowed here'
        ), follow_redirects=True)
        assert 'No entries here so far' not in rv.data
        assert '&lt;Hello&gt;' in rv.data
        assert '<strong>HTML</strong> allowed here' in rv.data

if __name__ == '__main__':
    unittest.main()