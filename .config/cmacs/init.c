#include <cmacs-api.h>

int main(int argc, char **argv)
{
	CmacsApi *api = cmacs_api_new(NULL);

	cmacs_message(api, "hello from init.c");
	cmacs_api_free(api);

	return 0;
}
